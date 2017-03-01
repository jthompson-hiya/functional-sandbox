package sandbox

import fs2._
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.ExecutionContext

object EffectfulUnfold extends App {

  def fetchMore(state: ProcessingState[Int]): Future[(ProcessingState[Int],Seq[Int])] = {
    def makeData(base: Int) = Seq(10*base,10*base+1,10*base+2)

    Future.successful {
      state match {
        case ProcessingStates.Starting => (ProcessingStates.Continuing(1),makeData(0))
        case ProcessingStates.Continuing(counter) =>
          if(counter >= 3) (ProcessingStates.Stopping,makeData(counter))
          else (ProcessingStates.Continuing(counter+1),makeData(counter))
        case ProcessingStates.Stopping => (ProcessingStates.Stopping,Seq.empty[Int])
      }
    }
  }

  implicit val strategy = Strategy.fromExecutionContext(implicitly[ExecutionContext])
  val initialState: ProcessingState[Int] = ProcessingStates.Starting
  val source = Stream.unfoldEval[Task,ProcessingState[Int],Seq[Int]](initialState){ state =>
    if(state == ProcessingStates.Stopping) Task.now(None)
    else Task.fromFuture(fetchMore(state)).map{ case(newState,newValues) => Some(newValues, newState) }
  }

  println(source.runLog.unsafeRun().flatten)

  sealed trait ProcessingState[+Int]
  object ProcessingStates {
    case object Starting extends ProcessingState[Nothing]
    case class Continuing[T](continuationToken: T) extends ProcessingState[T]
    case object Stopping extends ProcessingState[Nothing]
  }
}
