package cse.applyunapply.apply

import org.scalatest.path

/**
  * Created by dnwiebe on 9/4/17.
  */

class A_Examples extends path.FunSpec {

  describe ("We all know how methods work:") {
    class Subject {
      def method (s: String, n: Int): String = {
        (0 until n).map {_ => s}.mkString
      }
    }

    describe ("You create an object of a class") {
      val subject = new Subject ()

      describe ("and call the method with judiciously-chosen parameters") {
        val result = subject.method ("abc", 4)

        it ("to get an expected result") {
          assert (result === "abcabcabcabc")
        }
      }
    }
  }

  describe ("You can overload methods, too:") {
    class Subject {
      def method (s: String, n: Int): String = {
        (0 until n).map {_ => s}.mkString
      }

      def method (s: String, t: String): String = {
        s.zip (t).flatMap {p => List (p._1, p._2)}.mkString
      }
    }

    describe ("Now the same object") {
      val subject = new Subject ()

      describe ("can have each method called with different parameters") {
        val oneResult = subject.method ("abc", 4)
        val anotherResult = subject.method ("abcdef", "uvwxyz")

        it ("and get two different results") {
          assert (oneResult === "abcabcabcabc")
          assert (anotherResult === "aubvcwdxeyfz")
        }
      }
    }
  }

  describe ("If you name your methods 'apply' instead of 'method'") {
    class Subject {
      def apply (s: String, n: Int): String = {
        (0 until n).map {_ => s}.mkString
      }

      def apply (s: String, t: String): String = {
        s.zip (t).flatMap {p => List (p._1, p._2)}.mkString
      }
    }

    val subject = new Subject ()

    describe ("they can still be called the same way") {
      val oneResult = subject.apply ("abc", 4)
      val anotherResult = subject.apply ("abcdef", "uvwxyz")

      it ("and produce the same results") {
        assert (oneResult === "abcabcabcabc")
        assert (anotherResult === "aubvcwdxeyfz")
      }
    }

    describe ("they can also be called without any dot or method name") {
      val oneResult = subject ("abc", 4)
      val anotherResult = subject ("abcdef", "uvwxyz")

      it ("and produce the same results") {
        assert (oneResult === "abcabcabcabc")
        assert (anotherResult === "aubvcwdxeyfz")
      }
    }
  }

  describe ("You can do the same thing with singleton objects") {
    object Subject {
      def apply (s: String, n: Int): String = {
        (0 until n).map {_ => s}.mkString
      }
    }

    it ("where you don't need to create an instance") {
      assert (Subject ("abc", 4) === "abcabcabcabc")
    }
  }
}
