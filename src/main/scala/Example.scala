package main

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.language.implicitConversions
import scalaz.{Coproduct, FreeAp, Free, ~>}
import scalaz.std.scalaFuture._
import scalaz.syntax.apply._

object Example {

  sealed trait Op[A]
  case class print(msg: String) extends Op[Unit]
  case class stall(ms: Int) extends Op[Unit]

  type Prog[A] = Free[Op, A]
  type ApProg[A] = FreeAp[Prog, A]
  type ApM[A] = Coproduct[Prog, ApProg, A]
  type ApMProg[A] = Free[ApM, A]

  implicit class FreeOps[A](op: Op[A]) {
    def free: Prog[A] = Free.liftF(op)
  }

  implicit class ProgOps[A](prog: Prog[A]) {
    def ap: ApProg[A] = FreeAp.lift(prog)
    def apM: ApMProg[A] = Free.liftF(Coproduct.left[ApProg](prog): ApM[A])
  }

  implicit class ApProgOps[A](prog: ApProg[A]) {
    def apM: ApMProg[A] = Free.liftF(Coproduct.right[Prog](prog): ApM[A])
  }

  def stallPrint(s: String): Prog[Unit] =
    for {
      _ <- stall(1500).free
      _ <- print(s).free
    } yield ()

  val mInterp = new (Op ~> Future) {
    def apply[A](op: Op[A]) = op match {
      case print(msg) => Future { println(msg) }
      case stall(ms) => Future { Thread.sleep(ms) }
    }
  }

  val apInterp = new (Prog ~> Future) {
    def apply[A](f: Prog[A]) = f.foldMap(mInterp)
  }

  val apMInterp = new (ApM ~> Future) {
    def apply[A](f: ApM[A]) =
      f.run.fold(_.foldMap(mInterp), _.foldMap(apInterp))
  }

  val apMProg = for {
    _ <- stallPrint("start").apM
    _ <- (stallPrint("foo").ap.apM *> stallPrint("bar").ap.apM)
  } yield ()

  val apProg = (stallPrint("foo").ap *> stallPrint("bar").ap)

  def run = apMProg.foldMap(apMInterp) // runs sequentially, even in applicative
  def runAp = apProg.foldMap(apInterp) // runs parallel
}
