object ParticipantB extends App {
  import akka.actor._
  import com.machinomy.consensus.Participant
  import com.machinomy.xicity.Identifier

  implicit val system = ActorSystem()

  val a = Identifier(90)
  val b = Identifier(99)

  val aNeighbors = Set(b)
  val bNeighbors = Set(a)

  val participantB = system.actorOf(Props(classOf[Participant], b, bNeighbors))
}
