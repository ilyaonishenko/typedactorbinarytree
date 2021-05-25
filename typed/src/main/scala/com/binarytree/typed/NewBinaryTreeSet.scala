package com.binarytree.typed

import akka.actor.typed.{ActorRef, ActorSystem, Behavior}
import akka.actor.typed.scaladsl.Behaviors


object NewBinaryTreeSet {
  trait Operation {
    def requester: ActorRef[OperationReply]
    def id: Int
    def elem: Int
  }
  trait OperationReply {
    def id: Int
  }
  case class Insert(requester: ActorRef[OperationReply], id: Int, elem: Int) extends Operation
  case class Contains(requester: ActorRef[OperationReply], id: Int, elem: Int) extends Operation

  case class ContainsResult(id: Int, result: Boolean) extends OperationReply
  case class OperationFinished(id: Int) extends OperationReply

  def actor(): Behavior[Operation] = {
    Behaviors.setup[Operation] { context =>
      val root = context.spawn(NewBinaryTreeNode.actor(0, initRemove = true), "root")
      Behaviors.receiveMessage[Operation] {
        case operation: Operation =>
          root ! operation
          Behaviors.same
      }
    }
  }
}

object NewBinaryTreeNode {
  import NewBinaryTreeSet._
  trait Position
  case object Left extends Position
  case object Right extends Position


  def actor(elem: Int, initRemove: Boolean, subtrees: Map[Position, ActorRef[Operation]] = Map.empty): Behavior[Operation] =
    Behaviors.receive { (context, message) =>
      def checkLeaf(searchFor: Int): Position =
        if (searchFor > elem) Right else Left

      context.log.info(subtrees.toString())

      message match {
        case Insert(requester, id, el) if elem == el && !initRemove =>
          requester ! OperationFinished(id)
          Behaviors.same
        case iReq@Insert(requester, id, el) =>
          val pos = checkLeaf(el)
          if (subtrees.contains(pos)) {
            subtrees(pos) ! iReq
            Behaviors.same
          } else {
            val newActor = context.spawn(actor(el, initRemove), s"$el")
            val newSub = subtrees ++ Map(pos -> newActor)
            requester ! OperationFinished(id)
            actor(elem, initRemove, newSub)
          }
        case Contains(requester, id, el) if el == elem && !initRemove =>
          requester ! ContainsResult(id, result = true)
          Behaviors.same
        case cReq@Contains(requester, id, el) =>
          val pos = checkLeaf(el)
          if (subtrees.contains(pos)) {
            subtrees(pos) ! cReq
            Behaviors.same
          } else {
            requester ! ContainsResult(id, result = false)
            Behaviors.same
          }
      }
    }
}

object TestTyped extends App {
  import NewBinaryTreeSet._
  import scala.concurrent.duration._
  import akka.event.Logging
  import akka.pattern.ask
  import akka.util.Timeout
  implicit val timeout: Timeout = 3.seconds

  case class Act(id: Int, el: Int) extends OperationReply
  case class Check(id: Int, el: Int) extends OperationReply

  def testActor(): Behavior[OperationReply] = {
    Behaviors.setup { context =>
      val treeSet = context.spawn(NewBinaryTreeSet.actor(), "binaryTreeSet")
      Behaviors.receiveMessage {
        case Act(id, el) =>
          treeSet ! Insert(context.self, id, el)
          context.log.info(s"Send Insert command with id: $id and elem: $el")
          Behaviors.same
        case Check(id, el) =>
          treeSet ! Contains(context.self, id, el)
          context.log.info(s"Send Contains command with id: $id and elem: $el")
          Behaviors.same
        case OperationFinished(id) =>
          context.log.info(s"Receive operation finished for id: $id")
          Behaviors.same
        case ContainsResult(id, result) =>
          context.log.info(s"Receive operation finished for id: $id, result: $result")
          Behaviors.same
      }
    }
  }

  val system = ActorSystem(testActor(), "binaryTreeExample")
  val actor: ActorRef[OperationReply] = system

  actor ! Act(0, 1)
  actor ! Act(1, 2)
  actor ! Act(2, 3)
  actor ! Check(1, 1)
  actor ! Check(3, 2)
  actor ! Check(4, 3)
  actor ! Check(5, 4)
}
