package com.util

import java.io.File

import com.BaseSpec

import scala.io.Source


class FileIOUtilTest extends BaseSpec {

  "io test" should {

    val rootPath = "C:\\Users\\Slava\\Private\\Projects\\ScalaPlaybox\\TestDirectory"
    val rootDirectory: File = new File(rootPath)

    def createDirectory(file: File): File = {
      if (!file.exists()) {
        try {
          file.mkdir()
        } catch {
          case e: Exception =>
            println(e.getLocalizedMessage)
        }
      }

      file
    }

    def filePath(fileName: String) = s"$rootPath\\$fileName"

    createDirectory(rootDirectory)

    val fileName1 = "test1.txt"
    val str11 = "This is file1"
    FileIOUtil.writeToFile(filePath(fileName1), str11)
    val str12 = "This is file1 again"
    FileIOUtil.appendToFile(filePath(fileName1), str12)

    val fileName2 = "test2.txt"
    val str21 = "This is file2"
    FileIOUtil.writeToFile(filePath(fileName2), str21)
    val str22 = "This is file2 again"
    FileIOUtil.appendToFile(filePath(fileName2), str22)

    val files: Seq[File] = rootDirectory.listFiles.toSeq

    def read(fileName: String): Seq[String] = {
      val file = files.find(_.getName == fileName).getOrElse(
        sys.error(s"No file $fileName found"))

      val source = Source.fromFile(file)
      source.getLines().toSeq
    }

    def toString(fileName: String): String = {
      read(fileName).mkString("\n")
    }

    "writing and appending to file" in {
      files.size shouldEqual 2
      toString(fileName1) shouldEqual s"$str11\n$str12"
      toString(fileName2) shouldEqual s"$str21\n$str22"
    }
  }

}
