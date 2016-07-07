package com.machinomy.consensus

import akka.actor.{Actor, ActorLogging, ActorRef, Cancellable}
import com.machinomy.xicity.Identifier
import scodec._
import com.machinomy.xicity.transport._
import com.github.nscala_time.time.Imports._
import com.machinomy.consensus.state.PNCounter
import scodec.bits.BitVector

import scala.concurrent.duration
import scala.util.Random

class Participant(identifier: Identifier, neighbors: Set[Identifier]) extends Actor with ActorLogging {
  val PROTOCOL = 2
  val QUANTUM = duration.FiniteDuration(3, duration.SECONDS)
  val EPSILON = 50

  var nodeActor: ActorRef = null
  var clientNodeActor: ActorRef = null

  import context.dispatcher

  var currentRow: Row = Row(DateTime.now().getMillis, PNCounter[Identifier, Production]())

  var measureTick: Cancellable = null

  override def preStart(): Unit = {
    nodeActor = context.actorOf(Node.props(identifier, self))
    clientNodeActor = context.actorOf(ClientNode.props(Node.Wrap(nodeActor, Parameters.default)))
  }

  override def receive: Receive = {
    case Node.IsReady() =>
      println("|||||--------------------> READY")
      measureTick = context.system.scheduler.schedule(QUANTUM, QUANTUM, self, Participant.MeasureTick)
    case m: Message.Shot =>
      if (m.protocol == PROTOCOL) {
        Codec.decode[Row](BitVector(m.text)).toOption.map { f: DecodeResult[Row] => f.value } match {
          case Some(row) =>
            println(s"|||||--------------------> Received ow at ${row.timestamp}: ${row.mapping.table}")
            println(s"|||||--------------------> While having ${currentRow.timestamp}: ${currentRow.mapping.table}")

            if ((row.timestamp - currentRow.timestamp).abs > EPSILON) {
              val timestamp = (row.timestamp + currentRow.timestamp)/2

              println(s"|||||--------------------> Our schedule is lagging")
              measureTick.cancel()
              val nextTickDelta = QUANTUM.toMillis + timestamp - List(currentRow.timestamp, row.timestamp).min
              measureTick = context.system.scheduler.schedule(duration.FiniteDuration(nextTickDelta, duration.MILLISECONDS), QUANTUM, self, Participant.MeasureTick)
              println(s"|||||--------------------> Adjusted MeasureTick")

              println(s"?????? ${currentRow.mapping.merge(row.mapping)}")
              currentRow = Row(timestamp, currentRow.mapping.merge(row.mapping))
              disseminate(currentRow)
            } else {
              currentRow = currentRow.copy(mapping =  currentRow.mapping.merge(row.mapping))
            }

            println(s"${DateTime.now()}|||||--------------------> Got new row at ${currentRow.timestamp}: ${currentRow.mapping.table}")
          /*if (row.mapping.get(identifier) != prevLastMeasure) {
            println(s"|||||--------------------> Received row contains ${row.mapping.get(identifier)} for me, expected $lastMeasure")
            val delta = implicitly[Numeric[Production]].minus(prevLastMeasure, row.mapping.get(identifier))
            val nextMapping = row.mapping.increment(identifier, delta)
            disseminate(row.timestamp, nextMapping)
            sendingTick.cancel()
            sendingTick = context.system.scheduler.schedule(2 * QUANTUM, QUANTUM, self, Participant.SendingTick)
          }*/
          case None => log.error(s"|||||--------------------> Achtung!!!! Got none instead of row")
        }
      }
    case Participant.SendingTick =>
      println(s"|||||--------------------> SendingTick")
    /*val counter = PNCounter[Identifier, Production]().increment(identifier, lastMeasure)
    disseminate(lastMeasureTime.getMillis, counter)*/
    case Participant.MeasureTick =>
      println(s"|||||--------------------> MeasureTick")
      val production = Production(Random.nextDouble(), cost)
      settle(currentRow.copy())
      currentRow = Row(DateTime.now.getMillis, PNCounter[Identifier, Production]().update(identifier, production))
      disseminate(currentRow)
    /*prevLastMeasure = lastMeasure.copy()
    lastMeasure = Production(Random.nextDouble(), cost)
    lastMeasureTime = DateTime.now()
    measureTick.cancel()
    measureTick = context.system.scheduler.scheduleOnce(QUANTUM, self, Participant.MeasureTick)
    println(s"|||||--------------------> Measured $lastMeasure")*/
  }

  def disseminate(row: Row): Unit = {
    /*val row = Row(timestamp, PNCounter[Identifier, Production]())
    row.mapping.table
    val nextMapping = row.mapping.update(Identifier(1), Production(3,4))
    val nextRow = row.copy(mapping = nextMapping)

    val next2Mapping = row.mapping.merge(nextMapping)*/
    val bytes = Codec.encode(row).toOption.get.toByteArray
    val exp = expiration()
    for (n <- neighbors) {
      clientNodeActor ! Message.Shot(identifier, n, PROTOCOL, bytes, exp)
    }
  }

  def settle(row: Row): Unit = {
    println(s"||||||||||> Last row at ${row.timestamp}: ${row.mapping.table}")
    println(s"Pre-Balance: ${row.mapping.value.volume}")
    val balance = -1 * row.mapping.value.volume
    val balanced = row.mapping.update(Identifier(-1), Production(balance, 40))
    println(s"================================================")
    println(s"||||||||||> Last row balanced: at ${row.timestamp}: ${balanced.table}")
    val producers: Map[Identifier, Production] = balanced.table.filter(_._2.volume < 0)
    val consumers = balanced.table.filter(_._2.volume >= 0)
    val averagePrice = producers.values.map(p => p.volume * p.cost).sum / producers.values.map(_.volume).sum
    println(s"AVERAGE PRICE: $averagePrice")

    for (destination <- producers.keys.toSet - identifier) {
      producers.get(destination) match {
        case Some(production) =>
          val moneys = -1 * production.volume * averagePrice
          println(s"Paying $moneys to $destination")
        case None =>
      }
    }
    println(s"================================================")
  }

  def nextRandom = Random.nextInt

  def expiration() = (DateTime.now.getMillis / 1000) + 20

  def cost: Double = Random.nextInt.toDouble
}

object Participant {
  object SendingTick
  object MeasureTick
}
