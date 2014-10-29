object session {
    def abs(x: Double) = if (x < 0) -x else x

    def sqrt(x: Double) = {

        def sqrtIter(guess: Double): Double =
            if (isGoodEnough(guess)) guess
            else sqrtIter(improve(guess))

        def isGoodEnough(guess: Double) =
            // original in lecture - fails on small and large values
            //abs(guess * guess - x) < 0.001
            // works now because is proportional to x
            abs(guess * guess - x) / x < 0.001

        def improve(guess: Double) =
            (guess + x / guess) / 2

        sqrtIter(1.0)
    }

    def main(args: Array[String]) {
        println("square root of 0.001")
        println(sqrt(0.001))
        println("square root of 0.1e-20")
        println(sqrt(0.1e-20))
        println("square root of 1.0e20")
        println(sqrt(1.0e20))
        println("square root of 1.0e50")
        println(sqrt(1.0e50))
    }
}
