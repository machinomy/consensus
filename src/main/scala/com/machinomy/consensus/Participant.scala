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

  var measureTick: Cancellable = null
  var lastMeasure: Production = Production(0, 0)
  var prevLastMeasure: Production = Production(0, 0)
  var lastMeasureTime: DateTime = DateTime.now
  val cost: Double = 1

  var canSend = false
  var sendingTick: Cancellable = null

  override def preStart(): Unit = {
    nodeActor = context.actorOf(Node.props(identifier, self))
    clientNodeActor = context.actorOf(ClientNode.props(Node.Wrap(nodeActor, Parameters.default)))
    measureTick = context.system.scheduler.scheduleOnce(duration.FiniteDuration(0, duration.SECONDS), self, Participant.MeasureTick)
  }

  override def receive: Receive = {
    case Node.IsReady() =>
      println("|||||--------------------> READY")
      canSend = true
      sendingTick = context.system.scheduler.schedule(duration.FiniteDuration(0, duration.SECONDS), QUANTUM, self, Participant.SendingTick)
    case m: Message.Shot =>
      if (m.protocol == PROTOCOL) {
        Codec.decode[Row](BitVector(m.text)).toOption.map { f: DecodeResult[Row] => f.value } match {
          case Some(row) =>
            println(s"|||||--------------------> Received $row")
            val theirTime = new DateTime(row.timestamp)
            if (theirTime > lastMeasureTime + EPSILON) {
              println(s"|||||--------------------> Their time is greater")
              val nextTickDelta = lastMeasureTime.getMillis + QUANTUM.toMillis - theirTime.getMillis
              measureTick.cancel()
              measureTick = context.system.scheduler.scheduleOnce(duration.FiniteDuration(nextTickDelta, duration.MILLISECONDS), self, Participant.MeasureTick)
              println(s"|||||--------------------> Adjusted MeasureTick")
            }
            if (row.mapping.get(identifier) != prevLastMeasure) {
              println(s"|||||--------------------> Received row contains ${row.mapping.get(identifier)} for me, expected $lastMeasure")
              val delta = implicitly[Numeric[Production]].minus(prevLastMeasure, row.mapping.get(identifier))
              val nextMapping = row.mapping.increment(identifier, delta)
              disseminate(row.timestamp, nextMapping)
              sendingTick.cancel()
              sendingTick = context.system.scheduler.schedule(2 * QUANTUM, QUANTUM, self, Participant.SendingTick)
            }
          case None => log.error(s"|||||--------------------> Achtung!!!! Got none instead of row")
        }
      }
    case Participant.SendingTick =>
      println(s"|||||--------------------> SendingTick")
      val counter = PNCounter[Identifier, Production]().increment(identifier, lastMeasure)
      disseminate(lastMeasureTime.getMillis, counter)
    case Participant.MeasureTick =>
      println(s"|||||--------------------> MeasureTick")
      prevLastMeasure = lastMeasure.copy()
      lastMeasure = Production(Random.nextDouble(), cost)
      lastMeasureTime = DateTime.now()
      measureTick.cancel()
      measureTick = context.system.scheduler.scheduleOnce(QUANTUM, self, Participant.MeasureTick)
      println(s"|||||--------------------> Measured $lastMeasure")
  }

  def disseminate(timestamp: Long, counter: PNCounter[Identifier, Production]): Unit = {
    val row = Row(timestamp, counter)
    val bytes = Codec.encode(row).toOption.get.toByteArray
    val exp = expiration()
    for (n <- neighbors) {
      clientNodeActor ! Message.Shot(identifier, n, PROTOCOL, bytes, exp)
    }
  }

  def nextRandom = Random.nextInt

  def expiration() = (DateTime.now.getMillis / 1000) + 20
}

object Participant {
  object SendingTick
  object MeasureTick
}
