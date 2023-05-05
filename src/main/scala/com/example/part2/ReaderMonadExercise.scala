package com.example.part2

import cats._
import cats.implicits._
import cats.data._

// 1. Define an EmailService module with a method sendEmail(address: String, text: String): Unit
// 2. Define a LiveEmailService trait with a dummy implementation for the module
// 3. Update Env and liveEnv to include this new module
// 4. Add an emailAddress field to the Person class and change the PersonRepository implementation accordingly
// 5. Add a saveAccount(account: Account): Unit method to the AccountRepository module
// 6. Add a dummy implementation for saveAccount in LiveAccountRepository
// 7. Implement an openAccount(accountId, ownerId) method that will create and save a new account,
// 8. and will notify the user via email. Use the reader monad, and the dependencies that you see fit.
// 9. Run the function using the live environment
object ReaderMonadExercise extends App {

  case class Person(id: Long, name: String, emailAddress: String)
  case class Account(id: Long, ownerId: Long)

  trait PersonModule {
    val personService: Service
    trait Service {
      def findById(id: Long): Person
    }
  }
  trait LivePersonService extends PersonModule {
    override val personService: Service = new Service {
      override def findById(id: Long): Person = Person(2, "Dmitri M Guha", "dmitri.guha@gmail.com")
    }
  }

  trait AccountModule {
    val accountService: Service
    trait Service {
      def findById(id: Long): Account
      def saveAccount(acc: Account): Unit
    }
  }
  trait LiveAccountService extends AccountModule {
    override val accountService: Service = new Service {
      override def findById(id: Long): Account = Account(id, 2)

      override def saveAccount(acc: Account): Unit = println(s"Account $acc saved successfully")
    }
  }

  trait EmailServiceModule {
    val emailService: Service
    trait Service {
      def sendEmail(address: String, text: String): Unit
    }
  }
  trait LiveEmailService extends EmailServiceModule {
    override val emailService: Service = new Service {
      override def sendEmail(address: String, text: String): Unit = println(s"Email sent to $address with content $text")
    }
  }

  type Env = PersonModule with AccountModule with EmailServiceModule
  val liveEnv: Env = new LivePersonService with LiveAccountService with LiveEmailService

  def openAccount(personId: Long, accountId: Long): Reader[Env, Account] = {
    for {
      pModule <- Reader(identity[PersonModule])
      accModule <- Reader(identity[AccountModule])
      emailModule <- Reader(identity[EmailServiceModule])

      person = pModule.personService.findById(personId)
      account = Account(accountId, person.id)
      _ = accModule.accountService.saveAccount(account)
      _ = emailModule.emailService.sendEmail(person.emailAddress, "Account Successfully created")
    } yield account
  }

  val account = openAccount(1, 12).run(liveEnv)
  println(s"Successfully created $account")
}
