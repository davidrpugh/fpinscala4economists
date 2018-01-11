import java.util.concurrent.Executors

import parallelism.Par

// reasonably generic sequential summation of integers...
def sum(numbers: Seq[Int]): Int = {
  numbers.foldLeft(0)((total, number) => total + number)
}

sum(List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))

Par.parSum(Vector(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))


val p = Par.parMap(List.range(1, 100))(math.sqrt(_))
Par.run(Executors.newFixedThreadPool(2))(p)