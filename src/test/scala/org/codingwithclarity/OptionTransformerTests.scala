package org.codingwithclarity

import org.scalatest.{FlatSpec, ShouldMatchers}

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global

class OptionTransformerTests extends FlatSpec with ShouldMatchers {

  implicit val futureMonad = new Monad[Future] {
    def map[A, B](value: Future[A])(f: (A) => B) = value.map(f)

    def flatMap[A, B](value: Future[A])(f: (A) => Future[B]) = value.flatMap(f)

    def pure[A](x: A): Future[A] = Future(x)
  }

  it should "be able to handle a map without mapping over the future first" in {

    val one = OptionTransformer(Future(Option(1)))

    val newFuture = one.map(_ + 1)

    val result = Await.result(newFuture.value, Duration.Inf)

    result should be (Option(2))
  }

  it should "return none if the value is not present" in {

    val noneOption: Option[Int] = None

    val none = OptionTransformer(Future(noneOption))

    val newFuture = none.map(_ + 1)

    val result = Await.result(newFuture.value, Duration.Inf)

    result should be (None)
  }

  it should "be able to map over multiple transformers with a for comprehension"  in {
    val one: OptionTransformer[Future, Int] = OptionTransformer(Future(Option(1)))
    val two: OptionTransformer[Future, Int] = OptionTransformer(Future(Option(2)))

    val summed = for {
      resultOne <- one
      resultTwo <- two
    } yield {
      resultOne + resultTwo
    }

    val result = Await.result(summed.value, Duration.Inf)

    result should be (Some(3))
  }

  it should "be able to map over multiple transformers with a None result"  in {
    val one: OptionTransformer[Future, Int] = OptionTransformer(Future(Option(1)))

    val noneOption: Option[Int] = None

    val none = OptionTransformer(Future(noneOption))

    val summed = for {
      resultOne <- one
      resultTwo <- none
    } yield {
      resultOne + resultTwo
    }

    val result = Await.result(summed.value, Duration.Inf)

    result should be (None)
  }

}
