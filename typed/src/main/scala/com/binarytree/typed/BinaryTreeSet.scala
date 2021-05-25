package com.binarytree.typed

import akka.actor.typed.{ActorRef, ActorSystem, Behavior}
import akka.actor.typed.scaladsl.Behaviors


object BinaryTreeSet {
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
      val root = context.spawn(BinaryTreeNode.actor(0, initRemove = true), "root")
      Behaviors.receiveMessage[Operation] {
        case operation: Operation =>
          root ! operation
          Behaviors.same
      }
    }
  }
}

object BinaryTreeNode {
  import BinaryTreeSet._
  trait Position
  case object Left extends Position
  case object Right extends Position

  def actor(elem: Int, initRemove: Boolean): Behavior[Operation] = {
    Behaviors.setup[Operation] { context =>
      var subtrees: Map[Position, ActorRef[Operation]] = Map[Position, ActorRef[Operation]]()
      var removed: Boolean = initRemove

      def checkLeaf(searchFor: Int): Position =
        if (searchFor > elem) Right else Left

      Behaviors.receiveMessage[Operation] {
        case Insert(requester, id, el) if elem == el && !removed =>
          requester ! OperationFinished(id)
          Behaviors.same
        case iReq@Insert(requester, id, el) =>
          val pos = checkLeaf(el)
          if (subtrees.contains(pos)) {
            subtrees(pos) ! iReq
            Behaviors.same
          } else {
            val newActor = context.spawn(actor(el, initRemove), s"$el")
            subtrees += (pos -> newActor)
            requester ! OperationFinished(id)
            Behaviors.same
          }
        case Contains(requester, id, el) if el == elem && !removed =>
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
}


