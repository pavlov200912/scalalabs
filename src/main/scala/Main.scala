import java.nio.file.{Files, Path, Paths, StandardCopyOption}

import cats.data.{IndexedStateT, StateT}
import cats.{Applicative, Functor, Id, Monad}
import cats.syntax.all._
import cats.instances.list._

import scala.collection.JavaConverters._


import scala.language.higherKinds

trait MkDir[F[_], Dir] {
  def mkDir(dir: Dir, name: String): F[Dir]
}

trait MkFile[F[_], Dir, File] {
  def mkFile(dir: Dir, name: String): F[File]
}

trait Printer[F[_], File] {
  def printName(file: File): F[Unit]
}

trait GetDirsFile[F[_], Dir, File] {
  def getDirsFile(dir: Dir): F[List[File]]
}

trait GetName[F[_], File] {
  def getFileName(list: File): F[String]
}

trait MoveFile[F[_], Dir, File] {
  def moveFile(file: File, dest: Dir): F[File]
}

class Program[F[_], Dir, File](implicit
                               F: Monad[F],
                               mkDir: MkDir[F, Dir],
                               mkFile: MkFile[F, Dir, File],
                               getName: GetName[F, File],
                               getDirsFile: GetDirsFile[F, Dir, File],
                               moveFile: MoveFile[F, Dir, File],
                               printer: Printer[F, File]) {
  def run(dir: Dir, dirName: String, fileNames: List[String]): F[Unit] = for {
    // CreateDir
    curDir <- mkDir.mkDir(dir,dirName)

    // CreateFiles
    _ <- fileNames.traverse(name => mkFile.mkFile(curDir, name))

    // Get all Files from Dir
    files <- getDirsFile.getDirsFile(curDir)

    // Print all Files
    _ <- files.traverse(file => printer.printName(file))

    // Get File names
    names <- files.traverse(file => getName.getFileName(file))

    // Get first letters
    chars = names.map(name => name.head)

    // Create dirs for first letters
    dirs <- chars.traverse(c => mkDir.mkDir(curDir, c.toString))

    // Move files to this dirs
    zipped = files.zip(dirs)
    _ <- zipped.traverse(pair => moveFile.moveFile(pair._1, pair._2))
  } yield ()

}

class RealFileSystem[F[_] : Applicative] extends MkDir[F, Path] with MkFile[F, Path, Path]
  with GetDirsFile [F, Path, Path] with GetName[F, Path] with MoveFile [F, Path, Path] {



  override def moveFile(file: Path, dest: Path): F[Path] = {
    Files.move(file, dest.resolve(file.getFileName), StandardCopyOption.REPLACE_EXISTING).pure[F]
  }

  override def getFileName(file: Path): F[String] = file.getFileName.toString.pure[F]

  override def mkDir(dir: Path, name: String): F[Path] = {
    if (!Files.exists(dir.resolve(name)))
      Files.createDirectories(dir.resolve(name)).pure[F]
    else
      dir.resolve(name).pure[F]
  }

  override def mkFile(dir: Path, name: String): F[Path] = {
    if (!Files.exists(dir.resolve(name)))
      Files.createFile(dir.resolve(name)).pure[F]
    else
      dir.resolve(name).pure[F]
  }

  override def getDirsFile(dir: Path): F[List[Path]] =
    Files.list(dir).filter(Files.isRegularFile(_)).iterator().asScala.toList.pure[F]
}

class ConsolePathPrinter[F[_] : Applicative] extends Printer[F, Path] {
  override def printName(file: Path): F[Unit] = println(file.getFileName).pure[F]
}


object TypeClasses {
  def main(args: Array[String]): Unit = {

    implicit val fs: RealFileSystem[Id] = new RealFileSystem[Id]
    implicit val printer: ConsolePathPrinter[Id] = new ConsolePathPrinter[Id]
    val program = new Program[Id, Path, Path]

    program.run(Paths.get("."), "test_dir", List("foo", "bar", "baz"))


  }
}