package sandbox

import scala.collection.immutable.Queue

object TopolocicalSort extends App {

  def unfold[T,S](state: S)(f: S => Option[(T,S)]): Stream[T] = f(state) match {
    case Some((value,newState)) => value #:: unfold(newState)(f)
    case None => Stream.empty
  }

  case class Edge[VERT](from: VERT, to: VERT)

  case class SortState[VERT](vertices: Set[VERT], edges: Set[Edge[VERT]], inDegree: Map[VERT,Int], queue: Queue[VERT])
  object SortState {
    def init[VERT](vertices: Set[VERT], edges: Set[Edge[VERT]]): SortState[VERT] = {
      val initInDegree = vertices.map(v => (v->0)).toMap
      val inDegree = edges.foldLeft(initInDegree){ case(acc,edge) => acc.updated(edge.to, acc(edge.to)+1) }
      val startingVerts = inDegree.collect{ case(v,c) if c == 0 => v }.toSeq
      SortState(
        vertices,
        edges,
        inDegree,
        Queue(startingVerts: _*)
      )
    }
  }

  def step[VERT](state: SortState[VERT]): Option[(VERT,SortState[VERT])] = {
    val dq = state.queue.dequeueOption
    dq match {
      case None => None
      case Some((currentVert,dequeuedQueue)) => {
        val (vertsToAddToQueue, newInDegree) =
          state.edges
            .filter{ case Edge(from,_) => from == currentVert }
            .foldLeft((List.empty[VERT], state.inDegree)) { case ((vertsToAddTOQueue,newInDegree), Edge(from,to)) =>
              newInDegree(to) match {
                case 0 => (vertsToAddTOQueue,newInDegree)
                case 1 => (to +: vertsToAddTOQueue, newInDegree.updated(to,0))
                case degree => (vertsToAddTOQueue,newInDegree.updated(to,degree-1))
              }
            }

        val newQueue = dequeuedQueue.enqueue(vertsToAddToQueue)

        Some((currentVert, state.copy(queue = newQueue, inDegree = newInDegree)))
      }
    }
  }

  def sortTopologically[VERT](vertices: Set[VERT], edges: Set[Edge[VERT]]): Seq[VERT] = {
    val initialState = SortState.init(vertices,edges)
    val stream = unfold(initialState)(step)

    stream.toList
  }

  val vertices = Set('A','B','C','D','E','F')
  val edges = Set(
    Edge('A','B'),
    Edge('A','D'),
    Edge('B','C'),
    Edge('C','D'),
    Edge('C','E'),
    Edge('D','E')
  )

  val sorted = sortTopologically(vertices,edges)

  println(sorted)
}
