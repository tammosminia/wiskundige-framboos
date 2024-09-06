import scala.collection.immutable.SortedSet
import scala.math.abs

case class Location(x: Int, y: Int) //(0,0) is top-left

enum Direction:
  case Up, Down, Left, Right

// Implementation left open for our imagination
trait Maze {
  def start: Location
  def finish: Location
  def possibleDirections(l: Location): Set[Direction]
}

case class Step(direction: Direction, destination: Location)
type Path = List[Step]

// Helper functions
def takeStep(from: Location, d: Direction): Step = ???
def randomElement[T](l: Iterable[T]): T = ???
def pathLeadsTo(maze: Maze, path: Path): Location = path.lastOption.map(_.destination).getOrElse(maze.start)
// determine possible next steps without going backwards
def possibleStepsAhead(maze: Maze, path: Path): Set[Step] = {
  val currentLocation = pathLeadsTo(maze, path)
  // All locations we can go from here
  val possibleSteps = maze.possibleDirections(currentLocation).map(takeStep(currentLocation, _))
  // Drop locations we've already visited
  possibleSteps.filterNot(path.map(_.destination).contains)
}


def randomWalk(maze: Maze): Path = {
  def solution(path: Path): Path = {
    val currentLocation = pathLeadsTo(maze, path)
    if (currentLocation == maze.finish) path
    else {
      // Choose the next step at random
      val direction = randomElement(maze.possibleDirections(currentLocation))
      val step = takeStep(currentLocation, direction)
      solution(path.appended(step))
    }
  }

  solution(List.empty)
}

def depthFirst(maze: Maze): Option[Path] = {
  /** @param path path from start to current location
   * @return Shortest path from start to finish. Can be None if we're at a dead end */
  def solution(path: Path): Option[Path] = {
    val currentLocation = pathLeadsTo(maze, path)
    if (currentLocation == maze.finish) Some(path)
    else {
      val steps = possibleStepsAhead(maze, path)
      // Determine all possible paths from here
      val solutions = steps.flatMap(step => solution(path.appended(step)))
      // Choose the shortest solution. If any, we could also be at a dead end.
      solutions.minByOption(_.length)
    }
  }

  solution(List.empty)
}

def breadthFirst(maze: Maze): Option[Path] = {
  def solution(paths: List[Path]): Option[Path] = {
    val newPaths = paths.flatMap { path =>
      possibleStepsAhead(maze, path)
        .map(path.appended)
    }
    if (newPaths.isEmpty) {
      None
    } else {
      newPaths
        .find(path => pathLeadsTo(maze, path) == maze.finish) // Return the solution if we have it
        .orElse(solution(newPaths)) // or go on with all paths of one step longer
    }
  }

  solution(List(List.empty))
}

def aStar(maze: Maze): Option[Path] = {
  // A quick estimation of the amount of steps needed to get from l to the finish
  // It may be lower than the actual length, but never higher
  def heuristic(l: Location): Int = abs(maze.finish.x - l.x) + abs(maze.finish.y - l.y)

  // Paths are sorted from shortest (distance + heuristic) to longest
  // This ensures we always consider the most promising paths first
  def solution(paths: SortedSet[(Int, Path)]): Option[Path] =
    paths.headOption.flatMap { case (_, path) =>
      if (pathLeadsTo(maze, path) == maze.finish) {
        Some(path)
      } else {
        val newPaths = possibleStepsAhead(maze, path).map { step =>
          val newPath = path.appended(step)
          val minValue = newPath.length + heuristic(step.destination)
          (minValue, newPath)
        }
        solution(paths.tail.concat(newPaths))
      }
    }

  implicit val order: Ordering[(Int, Path)] = Ordering.by(_._1)
  solution(SortedSet((Int.MaxValue, List.empty)))
}