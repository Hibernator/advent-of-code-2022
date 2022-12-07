package ch.hibernator.adventofcode

import scala.collection.mutable
import scala.util.chaining.scalaUtilChainingOps

object Day7 extends SolutionBase:
  override def day: Int = 7

  case class Directory(
      name: String,
      files: mutable.Set[File],
      directories: mutable.Set[Directory],
      parent: Option[Directory] = None
  ):
    def size: Int = files.map(_.size).sum + directories.map(_.size).sum

    def addFile(file: File): Boolean = this.files.add(file)

    def addDir(dir: Directory): Boolean = this.directories.add(dir)
    override def toString: String = s"$name $size"

  case class File(name: String, size: Int)

  val rootDir = Directory("/", mutable.Set(), mutable.Set(), None)

  // build directory structure
  input.tail.foldLeft(rootDir) { (currentDir, line) =>
    if line.startsWith("$") then
      val parts = line.split(" ")
      val command = parts(1)
      if command == "cd" then
        val whereDir = parts.last
        if whereDir == ".." then currentDir.parent.get
        else currentDir.directories.find(_.name == whereDir).get
      else currentDir
    else if line.startsWith("dir") then
      val newDirName = line.split(" ").last
      val newDir = Directory(newDirName, mutable.Set(), mutable.Set(), Some(currentDir))
      currentDir.addDir(newDir)
      currentDir
    else
      val (fileSize, fileName) = line.split(" ").pipe(parts => (parts.head.toInt, parts.last))
      val newFile = File(fileName, fileSize)
      currentDir.addFile(newFile)
      currentDir
  }

  val allDirs: mutable.Buffer[Directory] = mutable.Buffer()

  def traverseDir(dir: Directory): Unit = {
    allDirs.append(dir)
    dir.directories.foreach(traverseDir)
  }

  traverseDir(rootDir)

  val result1 = allDirs.filter(_.size <= 100000).map(_.size).sum
  println(result1)

  val currentEmptySpace = 70000000 - rootDir.size
  val needToFree = 30000000 - currentEmptySpace
  val dirToDelete = allDirs.filter(_.size >= needToFree).minBy(_.size)
  val result2 = dirToDelete.size
  println(result2)
