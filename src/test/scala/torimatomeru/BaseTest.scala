package torimatomeru

import org.parboiled2.ParseError
import org.scalatest.FunSpec
import java.util.concurrent.{ ThreadFactory, TimeUnit, TimeoutException }

trait BaseTest extends FunSpec {
  val scheduler = java.util.concurrent.Executors.newSingleThreadScheduledExecutor(new ThreadFactory {
    def newThread(r: Runnable) = {
      val res = new Thread(r, "Test timeout checker")
      res.setDaemon(true)
      res
    }
  })

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
          throw e
      } finally {
        timer.cancel(true)
      }
    }
  }

}