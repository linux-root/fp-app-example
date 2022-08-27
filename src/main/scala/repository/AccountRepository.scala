package repository

import cats.effect.IO
import repository.AccountRepository.Account

import scala.collection.mutable

trait AccountRepository {
  def saveAccount(email: String, password: String): IO[Unit]

  def getAccount(email: String): IO[Option[Account]]

  def isAccountExist(email: String): IO[Boolean] = getAccount(email).map(_.nonEmpty)
}

object AccountRepository {
  case class Account(email: String, password: String)

  def inMemoryRepository: AccountRepository = new AccountRepository {

    val memory: mutable.Map[String, String] = mutable.Map.empty[String, String]

    override def saveAccount(email: String, password: String): IO[Unit] = IO {
      memory += (email -> password)
    }

    override def getAccount(email: String): IO[Option[Account]] = IO {
      memory.get(email).map(password => Account(email, password))
    }
  }
}
