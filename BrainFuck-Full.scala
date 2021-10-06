
@main def evalFromFile(fileName: String) =
  val code = io.Source.fromFile(fileName).mkString
  val size = code.size
  var tempCode = code
  var codePtr = 0
  var cellPtr = 0
  val cells = Array.fill(30_000){0}

  def moveLoopPtr(direction: Int) =
    var count = 1;
    codePtr = tempCode.indexWhere(x => {
      count += Map('[' -> direction, ']' -> -direction).getOrElse(x, 0)
      count == 0
    }, codePtr + 1)
    1 > 1

  while (code.size != codePtr) {
    code(codePtr) match
      case '+' => cells(cellPtr) += 1
      case '-' => cells(cellPtr) -= 1
      case '>' => cellPtr += 1
      case '<' => cellPtr -= 1
      case '[' =>
        tempCode = code;
        cells(cellPtr) == 0 && moveLoopPtr(1)
      case ']' =>
        tempCode = code.reverse
        codePtr = code.size - 1 - codePtr
        cells(cellPtr) != 0 && moveLoopPtr(-1)
        codePtr = code.size - 1 - codePtr
      case '.' => print((cells(cellPtr) & 255).toChar)
      case ',' => cells(cellPtr) = Console.in.read()
      case _ => 0
    codePtr += 1
  }
