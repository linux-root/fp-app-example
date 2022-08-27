import cats.effect.{ExitCode, IO, IOApp}
import repository.AccountRepository
import ui.CLI

object Main extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = {
    val repo = AccountRepository.inMemoryRepository
    CLI.start(repo).as(ExitCode.Success)
  }
}
