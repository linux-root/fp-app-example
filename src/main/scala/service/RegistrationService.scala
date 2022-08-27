package service

import cats.data.Reader
import cats.effect.IO
import repository.AccountRepository

object RegistrationService {
  sealed trait Result {
    def message: String
  }

  case object InvalidPassword extends Result {
    override def message: String = "Your password must have at least 6 characters"
  }

  case class InvalidEmail(email: String) extends Result {
    override def message: String = s"$email is invalid email"
  }

  case class AccountAlreadyExist(email: String) extends Result {
    override def message: String = s"your account $email already exist. Please login"
  }

  case object RegisterSuccess extends Result {
    override def message: String = "Register successfully"
  }

  case object LoginSuccess extends Result {
    override def message: String = "Login successfully"
  }

  case object WrongPassword extends Result {
    override def message: String = "wrong password"
  }

  case class AccountNotExist(email: String) extends Result {
    override def message: String = s"Account $email doesn't exist"
  }

  def register(email: String, password: String): Reader[AccountRepository, IO[Result]] = Reader { repo =>
    val emailPattern = "\\S+@\\S+\\.\\S+".r
    if (!emailPattern.matches(email)) {
      IO.pure(InvalidEmail(email))
    } else if (password.length < 6) {
      IO.pure(InvalidPassword)
    } else for {
      alreadyExist <- repo.isAccountExist(email)
      result <- if (alreadyExist)
        IO.pure(AccountAlreadyExist(email))
      else
        repo.saveAccount(email, password).as(RegisterSuccess)
    } yield result
  }

  def login(email: String, password: String): Reader[AccountRepository, IO[Result]] = Reader { repo =>
    repo.getAccount(email).map {
      case None =>
        AccountNotExist(email)
      case Some(acc) =>
        if (acc.password != password) {
          WrongPassword
        } else {
          LoginSuccess
        }
    }
  }
}
