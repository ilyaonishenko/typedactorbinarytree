package com.binarytree.classic

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.event.LoggingAdapter
import akka.util.Timeout

import scala.collection.immutable.Queue

object BinaryTreeSet {
  trait Operation {
    def requester: ActorRef
    def id: Int
    def elem: Int
  }

  trait OperationReply {
    def id: Int
  }

  case class Insert(requester: ActorRef, id: Int, elem: Int) extends Operation
  case class Contains(requester: ActorRef, id: Int, elem: Int) extends Operation
//  case class Remove(requester: ActorRef, id: Int, elem: Int) extends Operation
//  case object GC
  case class ContainsResult(id: Int, result: Boolean) extends OperationReply
  case class OperationFinished(id: Int) extends OperationReply

  class BinaryTreeSet extends Actor {
    def createRoot: ActorRef = context.actorOf(BinaryTreeNode.props(0, initiallyRemoved = true))
    var root: ActorRef = createRoot
    var pendingQueue: Queue[Operation] = Queue.empty[Operation]

    def receive: Receive = normal

    val normal: Receive = {
      case op: Operation => root ! op
//      case GC => ???
    }

//    def garbageCollecting(newRoot: ActorRef): Receive = ???
  }

  object BinaryTreeNode {
    trait Position
    case object Left extends Position
    case object Right extends Position

//    case class CopyTo(treeNode: ActorRef)
//    case object CopyFinished

    def props(elem: Int, initiallyRemoved: Boolean): Props = Props(new BinaryTreeNode(elem, initiallyRemoved))
  }

  class BinaryTreeNode(val elem: Int, initiallyRemoved: Boolean) extends Actor {
    import BinaryTreeNode._
    var subtrees: Map[Position, ActorRef] = Map[Position, ActorRef]()
    var removed: Boolean = initiallyRemoved

    def receive: Receive = normal

    val normal: Receive = {
      case Insert(requester, id, el) if elem == el && !removed =>
        requester ! OperationFinished(id)
      case iReq @ Insert(requester, id, el) =>
        val pos = checkLeaf(el)
        if (subtrees.contains(pos)) {
          subtrees(pos) ! iReq
        } else {
          subtrees += (pos -> context.actorOf(BinaryTreeNode.props(el, initiallyRemoved = false)))
          requester ! OperationFinished(id)
        }
      case Contains(requester, id, el) if el == elem && !removed =>
        requester ! ContainsResult(id, result = true)
      case cReq @ Contains(requester, id, el) =>
        val pos = checkLeaf(el)
        if (subtrees.contains(pos)) {
          subtrees(pos) ! cReq
        } else requester ! ContainsResult(id, result = false)
    }

    private def checkLeaf(searchFor: Int): Position =
      if (searchFor > elem) Right else Left

//    def copying(expected: Set[ActorRef], insertConfirmed: Boolean): Receive = ???
  }
}


object TestClassic extends App {
  import BinaryTreeSet._
  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.duration._
  import akka.event.Logging
  import akka.pattern.ask
  implicit val timeout: Timeout = 3.seconds

  case class Act(id: Int, el: Int)
  case class Check(id: Int, el: Int)

  class TestActor() extends Actor {
    val log: LoggingAdapter = Logging(context.system, this)
    override def receive: Receive = {
      case Act(id, el) =>
        binaryTreeSet ! Insert(self, id, el)
        log.info(s"Send Insert command with id: $id and elem: $el")
      case Check(id, el) =>
        binaryTreeSet ! Contains(self, id, el)
        log.info(s"Send Contains command with id: $id and elem: $el")
      case OperationFinished(id) =>
        log.info(s"Receive operation finished for id: $id")
      case ContainsResult(id, result) =>
        log.info(s"Receive operation finished for id: $id, result: $result")
    }
  }

  val actorSystem = ActorSystem()
  val binaryTreeSet = actorSystem.actorOf(Props[BinaryTreeSet]())
  val testActor = actorSystem.actorOf(Props[TestActor]())

  testActor ! Act(0, 1)
  testActor ! Act(1, 2)
  testActor ! Act(2, 3)
  testActor ! Check(3, 1)
  testActor ! Check(4, 2)
  testActor ! Check(5, 3)
  testActor ! Check(6, 4)
}
