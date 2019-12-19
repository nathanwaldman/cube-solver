import scala.collection.mutable.ListBuffer

object Main2 extends App {
    val start = System.currentTimeMillis()
    val size = 5
    val v: Array[Boolean] = Array.ofDim(size * size * size)
    val solution: ListBuffer[Piece] = ListBuffer()

    case class Extent(description: String, delta: Int) {
        override def toString: String = description
    }

    case class Piece(origin: Int, extent: Extent, bump: Int) {
        override def toString: String = {
            val (ox, oy, oz) = toCoordinates(origin)
            val (bx, by, bz) = toCoordinates(bump)
            s"$ox,$oy,$oz;$extent;$bx,$by,$bz"
        }
    }

    private def toCoordinates(i: Int): (Int, Int, Int) = (i % size, (i / size) % size, i / (size * size))

    private def fromCoordinates(x: Int, y: Int, z: Int): Int = x + size * y + size * size * z

    private def areUnset(i: Int, e: Extent): Boolean =
        !v(e.delta * 1 + i) &&
        !v(e.delta * 2 + i) &&
        !v(e.delta * 3 + i)

    private def set(piece: Piece, to: Boolean) {
        v(piece.extent.delta * 0 + piece.origin) = to
        v(piece.extent.delta * 1 + piece.origin) = to
        v(piece.extent.delta * 2 + piece.origin) = to
        v(piece.extent.delta * 3 + piece.origin) = to
        v(piece.bump) = to
    }

    private def attemptToSolveWith(piece: Piece) {
        set(piece, to = true)
        solution.append(piece)
        findSolutions(findNextOpenPosition(piece.origin))
        solution.remove(solution.length - 1)
        set(piece, to = false)
    }

    private def findNextOpenPosition(i: Int): Int = {
        var nextPosition: Int = i + 1
        while(nextPosition != max && v(nextPosition)) {
            nextPosition += 1
        }
        nextPosition
    }

    // TODO Do we need negative extents?
    val xPos = Extent("+X", 1)
    val yPos = Extent("+Y", size)
    val zPos = Extent("+Z", size * size)

    val max = size * size * size

    findSolutions(i = 0)

    def findSolutions(i: Int) {
        if (i == max) {
            println(s"Found solution after ${(System.currentTimeMillis() - start) / (1000 * 60)} minutes.  Solution: ${solution.mkString(" ")}")
            return
        }

        var extent: Extent = xPos
        var bump: Int = -1

        if (!v(i)) {
            val (x, y, z) = toCoordinates(i)
            extent = xPos
            if (x + 3 < size && areUnset(i, extent)) {
                if (y + 1 < size) {
                    bump = fromCoordinates(x + 1, y + 1, z)
                    if (!v(bump)) {
                        attemptToSolveWith(Piece(i, extent, bump))
                    }
                    bump = fromCoordinates(x + 2, y + 1, z)
                    if (!v(bump)) {
                        attemptToSolveWith(Piece(i, extent, bump))
                    }
                }
                if (z + 1 < size) {
                    bump = fromCoordinates(x + 1, y, z + 1)
                    if (!v(bump)) {
                        attemptToSolveWith(Piece(i, extent, bump))
                    }
                    bump = fromCoordinates(x + 2, y, z + 1)
                    if (!v(bump)) {
                        attemptToSolveWith(Piece(i, extent, bump))
                    }
                }
                if (y - 1 >= 0) {
                    bump = fromCoordinates(x + 1, y - 1, z)
                    if (!v(bump)) {
                        attemptToSolveWith(Piece(i, extent, bump))
                    }
                    bump = fromCoordinates(x + 2, y - 1, z)
                    if (!v(bump)) {
                        attemptToSolveWith(Piece(i, extent, bump))
                    }
                }
                if (z - 1 >= 0) {
                    bump = fromCoordinates(x + 1, y, z - 1)
                    if (!v(bump)) {
                        attemptToSolveWith(Piece(i, extent, bump))
                    }
                    bump = fromCoordinates(x + 2, y, z - 1)
                    if (!v(bump)) {
                        attemptToSolveWith(Piece(i, extent, bump))
                    }
                }
            }
            extent = yPos
            if (y + 3 < size && areUnset(i, extent)) {
                if (x + 1 < size) {
                    bump = fromCoordinates(x + 1, y + 1, z)
                    if (!v(bump)) {
                        attemptToSolveWith(Piece(i, extent, bump))
                    }
                    bump = fromCoordinates(x + 1, y + 2, z)
                    if (!v(bump)) {
                        attemptToSolveWith(Piece(i, extent, bump))
                    }
                }
                if (z + 1 < size) {
                    bump = fromCoordinates(x, y + 1, z + 1)
                    if (!v(bump)) {
                        attemptToSolveWith(Piece(i, extent, bump))
                    }
                    bump = fromCoordinates(x, y + 2, z + 1)
                    if (!v(bump)) {
                        attemptToSolveWith(Piece(i, extent, bump))
                    }
                }

                if (x - 1 >= 0) {
                    bump = fromCoordinates(x - 1, y + 1, z)
                    if (!v(bump)) {
                        attemptToSolveWith(Piece(i, extent, bump))
                    }
                    bump = fromCoordinates(x - 1, y + 2, z)
                    if (!v(bump)) {
                        attemptToSolveWith(Piece(i, extent, bump))
                    }
                }
                if (z - 1 >= 0) {
                    bump = fromCoordinates(x, y + 1, z - 1)
                    if (!v(bump)) {
                        attemptToSolveWith(Piece(i, extent, bump))
                    }
                    bump = fromCoordinates(x, y + 2, z - 1)
                    if (!v(bump)) {
                        attemptToSolveWith(Piece(i, extent, bump))
                    }
                }
            }
            extent = zPos
            if (z + 3 < size && areUnset(i, extent)) {
                if (x + 1 < size) {
                    bump = fromCoordinates(x + 1, y, z + 1)
                    if (!v(bump)) {
                        attemptToSolveWith(Piece(i, extent, bump))
                    }
                    bump = fromCoordinates(x + 1, y, z + 2)
                    if (!v(bump)) {
                        attemptToSolveWith(Piece(i, extent, bump))
                    }
                }
                if (y + 1 < size) {
                    bump = fromCoordinates(x, y + 1, z + 1)
                    if (!v(bump)) {
                        attemptToSolveWith(Piece(i, extent, bump))
                    }
                    bump = fromCoordinates(x, y + 1, z + 2)
                    if (!v(bump)) {
                        attemptToSolveWith(Piece(i, extent, bump))
                    }
                }
                if (x - 1 >= 0) {
                    bump = fromCoordinates(x - 1, y, z + 1)
                    if (!v(bump)) {
                        attemptToSolveWith(Piece(i, extent, bump))
                    }
                    bump = fromCoordinates(x - 1, y, z + 2)
                    if (!v(bump)) {
                        attemptToSolveWith(Piece(i, extent, bump))
                    }
                }
                if (y - 1 >= 0) {
                    bump = fromCoordinates(x, y - 1, z + 1)
                    if (!v(bump)) {
                        attemptToSolveWith(Piece(i, extent, bump))
                    }
                    bump = fromCoordinates(x, y - 1, z + 2)
                    if (!v(bump)) {
                        attemptToSolveWith(Piece(i, extent, bump))
                    }
                }
            }
        }
    }
}
