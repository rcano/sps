package torimatomeru

import org.parboiled2._
import org.scalatest.FunSpec
import java.util.concurrent.{ ThreadFactory, TimeUnit, TimeoutException }
import shapeless.HList
import org.parboiled2.Parser.DeliveryScheme

trait BaseTest extends FunSpec {
  val scheduler = java.util.concurrent.Executors.newSingleThreadScheduledExecutor(new ThreadFactory {
    def newThread(r: Runnable) = {
      val res = new Thread(r, "Test timeout checker")
      res.setDaemon(true)
      res
    }
  })

  def ruleSucceeds[L <: HList](input: ParserInput)(selector: ScalaSyntax => RuleN[L]) = {
    val s = new ScalaSyntax(input)
    val res = s.__run(selector(s))(DeliveryScheme.Try)
    res.recover { case e: ParseError => println(s.formatError(e) + "\n" + e.formatTraces) }
    assert(res.isSuccess)
    assert(s.cursor === s.input.length)
  }
  def ruleFails[L <: HList](input: ParserInput)(selector: ScalaSyntax => RuleN[L]) = {
    val s = new ScalaSyntax(input)
    val res = s.__run(selector(s))(DeliveryScheme.Try)
    assert(s.cursor === 0)
    assert(res.isFailure)
  }

  def itShould(name: String)(f: => Unit) {
    it(s"Should $name") {
      note(s"Running: $name")

      //the following thread hack is done because sometimes parsers can go awry bigtime.
      //so when a timeout is reached, we destroy the thread, print the error and die. 
      val runnerThread = Thread.currentThread()
      val timer = scheduler.schedule(new Runnable {
        def run: Unit = {
          println(s"Test '$name' timed out, stopping.")
          runnerThread.stop()
        }
      }, 3, TimeUnit.SECONDS)
      try f
      catch {
        case e: StackOverflowError =>
          alert(s"Test '$name' failed with stack overflow")
          fail(e)
      } finally {
        timer.cancel(true)
      }
    }
  }

}