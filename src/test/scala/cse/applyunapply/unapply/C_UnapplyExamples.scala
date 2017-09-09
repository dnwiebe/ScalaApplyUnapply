package cse.applyunapply.unapply

import java.text.SimpleDateFormat
import java.util.Date

import org.scalatest.path

/**
  * Created by dnwiebe on 9/4/17.
  */

class C_UnapplyExamples extends path.FunSpec {
  describe ("We've seen how pattern matching works on Options.") {

    val discountClub = Map ("Tracy" -> 25, "John" -> 20, "Patrick" -> 40)

    def discountedPrice (fullPrice: Int, name: String): Int = {
      val rate = discountClub.get (name) match {
        case Some (memberRate) => memberRate
        case None => 0
      }
      fullPrice * (100 - rate) / 100
    }

    it ("Tracy gets a quarter off") {
      assert (discountedPrice (1000, "Tracy") === 750)
    }

    it ("John gets a fifth off") {
      assert (discountedPrice (1000, "John") === 800)
    }

    it ("Maximilian doesn't get anything off") {
      assert (discountedPrice (1000, "Maximilian") === 1000)
    }
  }

  describe ("We've seen how pattern matching works on case classes.") {

    case class Account (balance: Int, established: Date, isPlatinum: Boolean)

    def calculateRate (account: Account, today: Date): Double = {
      account match {
        case Account (_, _, isPlatinum) if isPlatinum => 0.05
        case Account (balance, established, _) => {
          val balanceFactor = 1.0 - (balance / 1000000.0)
          val ageFactor = 1.0 - ((today.getTime - established.getTime) / 31536000000.0)
          0.20 * balanceFactor * ageFactor
        }
      }
    }

    describe ("A brand-new Platinum account") {
      val result = calculateRate (Account (1000, mmddyyyy ("08012017"), true), mmddyyyy ("08012017"))

      it ("has a five-percent rate") {
        assert (result === 0.05)
      }
    }

    describe ("A month-old non-Platinum account with $1000") {
      val result = calculateRate (Account (100000, mmddyyyy ("07012017"), false), mmddyyyy ("08012017"))

      it ("has a higher rate") {
        assert (result > 0.16)
      }
    }
  }

  describe ("But we can do the same things on our own classes, thanks to unapply ().") {
    class Account (val balance: Int, val established: Date, val isPlatinum: Boolean)

    object Account {
      def unapply (account: Account): Option[(Int, Date, Boolean)] = {
        Some ((account.balance, account.established, account.isPlatinum))
      }
    }

    def calculateRate (account: Account, today: Date): Double = {
      account match {
        case Account (_, _, isPlatinum) if isPlatinum => 0.05
        case Account (balance, established, _) => rateFor (balance, established, today)
      }
    }

    describe ("A brand-new Platinum account") {
      val result = calculateRate (new Account (1000, mmddyyyy ("08012017"), true), mmddyyyy ("08012017"))

      it ("has a five-percent rate") {
        assert (result === 0.05)
      }
    }

    describe ("A month-old non-Platinum account with $1000") {
      val result = calculateRate (new Account (100000, mmddyyyy ("07012017"), false), mmddyyyy ("08012017"))

      it ("has a higher rate") {
        assert (result > 0.16)
      }
    }
  }

  describe ("But we don't want to code that, because case classes give it to us for free.") {
    describe ("Let's get a little creative: for example, an account that doesn't exist before its establishment") {
      class Account (val balance: Int, val established: Date, val isPlatinum: Boolean)

      object Account {
        def unapply (account: Account): Option[(Int, Date, Boolean)] = {
          account.established.after (new Date ()) match {
            case true => None
            case false => Some ((account.balance, account.established, account.isPlatinum))
          }
        }
      }

      def calculateRate (account: Account, today: Date): Option[Double] = {
        account match {
          case Account (_, _, isPlatinum) if isPlatinum => Some (0.05)
          case Account (balance, established, _) => Some (rateFor (balance, established, today))
          case _ => None
        }
      }

      describe ("An old account") {
        val result = calculateRate (new Account (1000, mmddyyyy ("01011957"), true), mmddyyyy ("08012017"))

        it ("has a rate") {
          assert (result.isDefined)
        }
      }

      describe ("A future account") {
        val result = calculateRate (new Account (1000, mmddyyyy ("01012100"), true), mmddyyyy ("08012017"))

        it ("has no rate") {
          assert (result.isEmpty)
        }
      }
    }

    describe ("Your unapply doesn't always have to return an Option of TupleX, if you only want one value") {
      class Account (val balance: Int, val established: Date, val isPlatinum: Boolean)

      object Account {
        def unapply (account: Account): Option[Int] = {
          if (account.established.after (new Date ())) None else Some (account.balance)
        }
      }

      def calculateBalance (account: Account): Option[Int] = {
        account match {
          case Account (balance) => Some (balance)
          case _ => None
        }
      }

      describe ("An old account") {
        val result = calculateBalance (new Account (1000, mmddyyyy ("01011957"), true))

        it ("has a balance") {
          assert (result.isDefined)
        }
      }

      describe ("A future account") {
        val result = calculateBalance (new Account (1000, mmddyyyy ("01012100"), true))

        it ("has no balance") {
          assert (result.isEmpty)
        }
      }
    }

    describe ("You don't even have to return an Option at all, if all you need is a Boolean") {
      class Account (val balance: Int, val established: Date, val isPlatinum: Boolean)

      object Account {
        def unapply (account: Account): Boolean = {
          account.established.before (new Date ())
        }
      }

      def determineExistence (account: Account): Boolean = {
        account match {
          case Account () => true
          case _ => false
        }
      }

      describe ("An old account") {
        val result = determineExistence (new Account (1000, mmddyyyy ("01011957"), true))

        it ("exists") {
          assert (result === true)
        }
      }

      describe ("A future account") {
        val result = determineExistence (new Account (1000, mmddyyyy ("01012100"), true))

        it ("doesn't exist yet") {
          assert (result === false)
        }
      }
    }

    describe ("One other form of unapply can be used when you don't know how many arguments will come out") {
      class Account (val balance: Int, val established: Date, val isPlatinum: Boolean)

      object Account {
        def unapplySeq (account: Account): Option[Seq[Int]] = {
          if (account.established.after (new Date ())) {
            None
          }
          else {
            val payment = 25000
            val identicalCount = account.balance / payment
            val lastPayment = account.balance - (payment * identicalCount)
            Some ((0 until identicalCount).map { _ => payment }.toList ::: List (lastPayment))
          }
        }
      }

      def calculateThreePayments (account: Account): Option[(Int, Int, Int, Boolean)] = {
        account match {
          case Account (p1) => Some ((p1, 0, 0, true))
          case Account (p1, p2) => Some ((p1, p2, 0, true))
          case Account (p1, p2, p3) => Some ((p1, p2, p3, true))
          case Account (p1, p2, p3, _*) => Some ((p1, p2, p3, false))
          case _ => None
        }
      }

      describe ("An account with less than $250") {
        val result = calculateThreePayments (new Account (20000, mmddyyyy ("01012017"), true))

        it ("requires one payment") {
          assert (result === Some ((20000, 0, 0, true)))
        }
      }

      describe ("An account with more than $500") {
        val result = calculateThreePayments (new Account (55000, mmddyyyy ("01012017"), true))

        it ("requires three payments") {
          assert (result === Some ((25000, 25000, 5000, true)))
        }
      }

      describe ("An account with $10000") {
        val result = calculateThreePayments (new Account (1000000, mmddyyyy ("01012017"), true))

        it ("can't be paid off in three payments") {
          assert (result === Some ((25000, 25000, 25000, false)))
        }
      }

      describe ("A future account") {
        val result = calculateThreePayments (new Account (1000, mmddyyyy ("01012100"), true))

        it ("requires no payments") {
          assert (result === None)
        }
      }
    }
  }

  private def mmddyyyy (strDate: String): Date = {
    new SimpleDateFormat ("MMddyyyy").parse (strDate)
  }

  private def rateFor (balance: Int, established: Date, today: Date): Double = {
    val balanceFactor = 1.0 - (balance / 1000000.0)
    val ageFactor = 1.0 - ((today.getTime - established.getTime) / 31536000000.0)
    0.20 * balanceFactor * ageFactor
  }
}