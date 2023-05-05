import cats._
import cats.implicits._
import cats.data._

val signReader: Reader[Int, String] = Reader(n => if(n > 0) "positive" else if(n < 0) "negative" else "zero")
signReader.run(0)

val parityReader: Reader[Int, String] = Reader(n => if(n %2 == 0) "even" else "odd")
parityReader.run(1)
parityReader.run(2)

val descriptionReader: Reader[Int, String] =
  for {
    sign <- signReader
    parity <- parityReader
  } yield (s"$sign and $parity")

descriptionReader.run(1)
descriptionReader.run(-2)

//env <- Reader((x: Int) => x) //ask(ing) the environment

val addOneReader: Reader[Int, Int] =
  for {
    env <- Reader(identity[Int])
  } yield env + 1

case class Person(id: Long, name: String)
case class Account(id: Long, ownerId: Long)

trait AccountRepository {
  def findAccountById(id: Long): Account
}
trait LiveAccountRepository extends AccountRepository {
  def findAccountById(id: Long): Account = Account(id, 2)
}
trait PersonRepository {
  def findPersonById(id: Long): Person
}
trait LivePersonRepository extends PersonRepository {
  def findPersonById(id: Long): Person = Person(id, "Some Name")
}

def findNextAccount(id: Long): Reader[AccountRepository, Account] =
  for {
    accountRepo <- Reader((accRepo: AccountRepository) => accRepo)
    account = accountRepo.findAccountById(id)
  } yield account

def findOwnerNameByAccountId(id: Long): Reader[Env, String] =
  for {
    accountRepo <- Reader((accRepo: AccountRepository) => accRepo)
    personRepo <- Reader((personRepo: PersonRepository) => personRepo)
    account = accountRepo.findAccountById(id)
    owner = personRepo.findPersonById(account.ownerId)
  } yield owner.name

type Env = PersonRepository with AccountRepository

val liveEnv: Env = new LivePersonRepository with LiveAccountRepository
findOwnerNameByAccountId(1).run(liveEnv)
