package ui

import cats.data.Reader
import cats.effect.IO
import repository.AccountRepository
import service.RegistrationService
import service.RegistrationService.{LoginSuccess, RegisterSuccess, Result}

import scala.concurrent.duration.DurationInt

/**
 * command line interface
 */
object CLI {
  case class Form(email: String, password: String)

  sealed trait Command

  case object Quit extends Command

  case object Login extends Command

  case object Register extends Command

  case object Ignore extends Command

  object Command {
    def apply(text: String): Command = text match {
      case "r" => Register
      case "l" => Login
      case "q" => Quit
      case _ => Ignore
    }
  }

  private val welcomeMessage = "\n  ██████  ███▄ ▄███▓ ▄▄▄       ██▀███  ▄▄▄█████▓▓█████  ██▀███      ██░ ██ ▓█████ ▄▄▄       ██▓  ▄▄▄█████▓ ██░ ██ \n▒██    ▒ ▓██▒▀█▀ ██▒▒████▄    ▓██ ▒ ██▒▓  ██▒ ▓▒▓█   ▀ ▓██ ▒ ██▒   ▓██░ ██▒▓█   ▀▒████▄    ▓██▒  ▓  ██▒ ▓▒▓██░ ██▒\n░ ▓██▄   ▓██    ▓██░▒██  ▀█▄  ▓██ ░▄█ ▒▒ ▓██░ ▒░▒███   ▓██ ░▄█ ▒   ▒██▀▀██░▒███  ▒██  ▀█▄  ▒██░  ▒ ▓██░ ▒░▒██▀▀██░\n  ▒   ██▒▒██    ▒██ ░██▄▄▄▄██ ▒██▀▀█▄  ░ ▓██▓ ░ ▒▓█  ▄ ▒██▀▀█▄     ░▓█ ░██ ▒▓█  ▄░██▄▄▄▄██ ▒██░  ░ ▓██▓ ░ ░▓█ ░██ \n▒██████▒▒▒██▒   ░██▒ ▓█   ▓██▒░██▓ ▒██▒  ▒██▒ ░ ░▒████▒░██▓ ▒██▒   ░▓█▒░██▓░▒████▒▓█   ▓██▒░██████▒▒██▒ ░ ░▓█▒░██▓\n▒ ▒▓▒ ▒ ░░ ▒░   ░  ░ ▒▒   ▓▒█░░ ▒▓ ░▒▓░  ▒ ░░   ░░ ▒░ ░░ ▒▓ ░▒▓░    ▒ ░░▒░▒░░ ▒░ ░▒▒   ▓▒█░░ ▒░▓  ░▒ ░░    ▒ ░░▒░▒\n░ ░▒  ░ ░░  ░      ░  ▒   ▒▒ ░  ░▒ ░ ▒░    ░     ░ ░  ░  ░▒ ░ ▒░    ▒ ░▒░ ░ ░ ░  ░ ▒   ▒▒ ░░ ░ ▒  ░  ░     ▒ ░▒░ ░\n░  ░  ░  ░      ░     ░   ▒     ░░   ░   ░         ░     ░░   ░     ░  ░░ ░   ░    ░   ▒     ░ ░   ░       ░  ░░ ░\n      ░         ░         ░  ░   ░                 ░  ░   ░         ░  ░  ░   ░  ░     ░  ░    ░  ░        ░  ░  ░\n                                                                                                                  "
  private val goodByeMessage = "\n██████╗ ██╗   ██╗███████╗    ██╗\n██╔══██╗╚██╗ ██╔╝██╔════╝    ██║\n██████╔╝ ╚████╔╝ █████╗      ██║\n██╔══██╗  ╚██╔╝  ██╔══╝      ╚═╝\n██████╔╝   ██║   ███████╗    ██╗\n╚═════╝    ╚═╝   ╚══════╝    ╚═╝\n                                \n                                \n                                \n                                \n                                \n                                \n                                \n                                "
  private val unknownCommandMessage = "    /\\_/\\           ___\n   = o_o =_______    \\ \\  -Unknown Command !-\n    __^      __(  \\.__) )\n(@)<_____>__(_____)____/"
  private val registerSuccessMessage = "Registered successfully\n           .'\\   /`.\n         .'.-.`-'.-.`.\n    ..._:   .-. .-.   :_...\n  .'    '-.(o ) (o ).-'    `.\n :  _    _ _`~(_)~`_ _    _  :\n:  /:   ' .-=_   _=-. `   ;\\  :\n:   :|-.._  '     `  _..-|:   :\n :   `:| |`:-:-.-:-:'| |:'   :\n  `.   `.| | | | | | |.'   .'\n    `.   `-:_| | |_:-'   .'\n      `-._   ````    _.-'\n          ``-------''"


  private val menu: Reader[AccountRepository, IO[Unit]] = Reader(identity[AccountRepository]).map { repo =>
    for {
      _ <- IO.println("\n\nPlease select 1 option:\n\n'r' to REGISTER\n'l' to LOGIN\n'q' to QUIT")
      cmd <- IO.readLine.map(Command(_))
      _ <- cmd match {
        case Quit => IO.println(goodByeMessage)
        case Ignore => IO.println(unknownCommandMessage) >> menu(repo)
        case Register => CLI.registerFlow(repo)
        case Login => CLI.loginFlow(repo)
      }
    } yield ()
  }

  private val displayForm: IO[Form] = for {
    _ <- IO.println("Enter your email:")
    email <- IO.readLine
    _ <- IO.println("Enter password:")
    password <- IO.readLine
  } yield Form(email, password)

  private def resetFlow(flow: IO[Unit], message: String): IO[Unit] = {
    IO.println(message) >> IO.println("reset form...") >> IO.sleep(1.second) >> flow
  }

  val registerFlow: Reader[AccountRepository, IO[Unit]] = {
    Reader(identity[AccountRepository]).map { repo =>
      for {
        form <- displayForm
        result <- RegistrationService.register(form.email, form.password).run(repo)
        _ <- result match {
          case RegisterSuccess =>
            IO.println(registerSuccessMessage) >> menu(repo)
          case r: Result =>
            resetFlow(registerFlow(repo), r.message)
        }
      } yield result
    }
  }

  val loginFlow: Reader[AccountRepository, IO[Unit]] = {
    Reader(identity[AccountRepository]).map { repo =>
      for {
        form <- displayForm
        result <- RegistrationService.login(form.email, form.password).run(repo)
        _ <- result match {
          case LoginSuccess =>
            IO.println(LoginSuccess.message) >> menu(repo)
          case r: Result =>
            resetFlow(loginFlow(repo), r.message)
        }
      } yield result
    }
  }

  def start: Reader[AccountRepository, IO[Unit]] =Reader(repo => IO.println(welcomeMessage) >> menu(repo))
}
