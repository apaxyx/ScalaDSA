package Algorithm.sort

object BubbleSort {

    def main(args: Array[String]): Unit = {
        val arr: Array[Int] = Array[Int](7, 6, 5, 4, 3, 2, 1)
        bubbleSort(arr)
        println(arr.mkString(","))
    }


    def bubbleSort(arr:Array[Int]): Unit ={
        for(i <- 0 until arr.length - 1){
            for(j <- 0 until arr.length - 1 - i){
                if(arr(j) > arr(j + 1)){
                    val temp = arr(j)
                    arr(j) = arr(j + 1)
                    arr(j + 1) = temp
                }
            }
        }
    }
}
