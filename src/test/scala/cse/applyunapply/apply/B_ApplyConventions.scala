package cse.applyunapply.apply

import org.scalatest.path

/**
  * Created by dnwiebe on 9/4/17.
  */

class B_ApplyConventions extends path.FunSpec {

  trait Make {
    val models: Set[String]
  }

  object Ford extends Make {
    override val models = Set ("Focus", "Fusion", "Taurus", "Mustang")
  }

  object Chevrolet extends Make {
    override val models = Set ("Cruze", "Malibu", "Impala", "Camaro")
  }

  object Toyota extends Make {
    override val models = Set ("Corolla", "Camry", "Prius", "Yaris")
  }

  class Car (val make: Make, val model: String)

  describe ("You can create a Car object in the obvious way, if you like") {
    val subject = new Car (Ford, "Fusion")

    it ("and it will act the way you expect") {
      assert (subject.make === Ford)
      assert (subject.model === "Fusion")
      assert (subject.make.models.contains (subject.model) === true)
    }

    describe ("but you can also create a Car that doesn't make any sense") {
      val invalid = new Car (Toyota, "Impala")

      it ("if you have low-level access to stuff you shouldn't") {
        assert (invalid.make.models.contains (invalid.model) === false)
      }
    }
  }

  describe ("Conventionally, apply () in a singleton object is used as a factory method") {

    object Car {
      def apply (model: String): Car = {
        new Car (List (Ford, Chevrolet, Toyota).find {_.models.contains (model)}.get, model)
      }
    }

    describe ("so that you can make a car based only on model") {
      val subject = Car ("Camaro")

      it ("and have the make automatically calculated") {
        assert (subject.make === Chevrolet)
      }
    }

    it ("but we'd rather see constructor exceptions than Options returned from factories") {
      try {
        Car ("Charger")
        fail ()
      }
      catch {
        case e: NoSuchElementException => assert (e.getMessage === "None.get")
      }
    }
  }

  describe ("Sometimes the difference between singleton-object apply () and a constructor can be confusing") {
    describe ("If we use the singleton-object apply on Array") {
      val subject: Array[Int] = Array (5)

      it ("we get a single-element array containing the number 5") {
        assert (subject === Array (5))
      }
    }

    describe ("If we use the constructor of Array") {
      val subject: Array[Int] = new Array (5)

      it ("we get a five-element array containing zeros") {
        assert (subject === Array (0, 0, 0, 0, 0))
      }
    }
  }

  describe ("Case classes come preconfigured with singleton-object factory methods that are less confusing") {
    case class Car (make: Make, model: String)

    describe ("The constructor and the factory method") {
      val oneResult = new Car (Ford, "Focus")
      val anotherResult = Car (Ford, "Focus")

      it ("are not the same code, but do exactly the same thing") {
        assert (oneResult === anotherResult)
      }
    }
  }
}
