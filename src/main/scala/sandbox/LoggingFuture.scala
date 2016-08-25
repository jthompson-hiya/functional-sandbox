package sandbox

import scalaz._
import scalaz.Scalaz._
import scala.concurrent.Future
import scala.concurrent.ExecutionContext
import scala.concurrent.Promise
import scala.concurrent.Await
import scala.concurrent.duration._

object LoggingFutureSupport {

  type LogEntry = String
  type LogWriter[A] = Writer[List[LogEntry],A]
  object LogWriter {
    def apply[T](msg: String, value: T): LogWriter[T] = Writer(List(msg),value)
    def apply[T](value: T): LogWriter[T] = Writer(List.empty,value)
  }

  type ErrorOr[A] = Throwable \/ A

  type LoggingErrorOr[A] = EitherT[LogWriter,Throwable,A]
  object LoggingErrorOr {
    def apply[T](msg: String, value: Throwable \/ T): LoggingErrorOr[T] = EitherT(LogWriter(msg,value))
    def apply[T](value: Throwable \/ T): LoggingErrorOr[T] = EitherT(LogWriter(value))
  }

  type FallableLoggingFuture[A] = WriterT[Future,List[LogEntry],A]
  object FallableLoggingFuture {
    def apply[T](msg: LogEntry, f: Future[T])(implicit ec: ExecutionContext): FallableLoggingFuture[T] =
      WriterT(f.map(value => (List(msg),value)))

    def apply[T](msg: LogEntry)(t: => T)(implicit ec: ExecutionContext): FallableLoggingFuture[T] = {
      val ft = Future(t)
      apply(msg,ft)
    }
    def apply[T](lt: => (LogEntry,T))(implicit ec: ExecutionContext): FallableLoggingFuture[T] = {
      val flt = Future(lt)
      WriterT(flt.map{ case(msg,t) => (List(msg),t) })
    }
  }

  type LoggingFuture[A] = EitherT[FallableLoggingFuture,Throwable,A]
  object LoggingFuture {
    def apply[T](flf: FallableLoggingFuture[Throwable \/ T])(implicit ec: ExecutionContext): LoggingFuture[T] =
      EitherT(flf)

    def apply[T](msg: LogEntry, sf: Future[Throwable \/ T])(implicit ec: ExecutionContext): LoggingFuture[T] =
      apply(FallableLoggingFuture(msg,sf))

    def apply[T](lst: => (LogEntry,Throwable \/ T))(implicit ec: ExecutionContext): LoggingFuture[T] =
      apply(FallableLoggingFuture(lst))

    def unsafe[T](msg: String, ff: Future[T])(implicit ec: ExecutionContext): LoggingFuture[T] =
      apply(msg, ff.map(_.right).recover{ case ex => ex.left })

    def unsafe[T](msg: LogEntry)(t: => T)(implicit ec: ExecutionContext): LoggingFuture[T] =
      unsafe((msg,t))

    def unsafe[T](lt: => (LogEntry,T))(implicit ec: ExecutionContext): LoggingFuture[T] = {
      apply(
        try {
          val (msg,t) = lt
          (msg,t.right)
        }
        catch {
          case ex: Throwable => ("failure!", ex.left)
        }
      )
    }
  }

  implicit class LoggingFutureOps[T](lf: LoggingFuture[T]) {
    def toFuture(implicit ec: ExecutionContext) = lf.run.run
  }
}

object LoggingFutureSandbox extends App {
  import LoggingFutureSupport._

  import ExecutionContext.Implicits.global

  def halfish(value: Int): LoggingFuture[Int] = LoggingFuture {
    if(value % 2 == 0) (s"$value looks fine", \/-(value/2))
    else (s"$value is a little odd", -\/(new IllegalArgumentException(s"$value is odd")))
  }

  {
    val res = for {
      a <- halfish(4)
      b <- halfish(6)
      c <- halfish(8)
    } yield (a+b+c)

    val (log,result) = Await.result(res.toFuture,10.seconds)
    println(s"Result: $result") // Result: \/-(9)
    println(s"Log:   $log")     // Log:    List(4 looks fine, 6 looks fine, 8 looks fine)
  }

  {
    val res = for {
      a <- halfish(4)
      b <- halfish(7)
      c <- halfish(8)
    } yield (a+b+c)

    val (log,result) = Await.result(res.toFuture,10.seconds)
    println(s"Result: $result") // Result: -\/(java.lang.IllegalArgumentException: 7 is odd)
    println(s"Log:   $log")     // Log:   List(4 looks fine, 7 is a little odd)
  }
}
