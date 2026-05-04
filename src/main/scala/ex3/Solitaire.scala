package ex3

object Solitaire:
  type Position = (Int, Int)
  type Path = List[Position]

  val height = 7
  val width = 7

  @main def run(): Unit =
    val startX = width / 2
    val startY = height / 2
    val initialPath = List((startX, startY))

    val allSolutions = placeMarks(initialPath)

    allSolutions.zipWithIndex foreach ((winningPath, index) => {
        println(s"\n sol ${index + 1}")
        println(render(winningPath, width, height))
      })

  def legalPositions(position: Position, path: Path): List[Position] =
    val (x, y) = position

    val possibleMoves = List(
      // vertical (same x)
      (x, y-3), (x, y+3),
      // horizontal (same y)
      (x-3, y), (x+3, y),
      // diagonal
      (x+2, y-2), (x+2, y+2), (x-2, y+2), (x-2, y-2)
    )

    possibleMoves.filter(pos => {
      val (newX, newY) = pos
      val isFree: Boolean = !path.contains(pos)
      val isInsideBounds: Boolean = newX >= 0 && newX < width && newY >= 0 && newY < height
      isFree && isInsideBounds
    })


  def placeMarks(current: Path): LazyList[Path] =
    if current.size == width * height then
      LazyList(current)
    else
      for
        legalPosition <- legalPositions(current.head, current).to(LazyList)
        solution <- placeMarks(legalPosition :: current)
      yield
        solution

  def render(solution: Seq[(Int, Int)], width: Int, height: Int): String =
    val reversed = solution.reverse
    val rows =
      for y <- 0 until height
          row = for x <- 0 until width
                    number = reversed.indexOf((x, y)) + 1
          yield if number > 0 then "%-2d ".format(number) else "X  "
      yield row.mkString
    rows.mkString("\n")

  //println(render(solution = Seq((0, 0), (2, 1)), width = 3, height = 3))