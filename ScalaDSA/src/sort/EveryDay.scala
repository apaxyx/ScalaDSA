package sort

import scala.collection.mutable.ArrayBuffer
import util.control.Breaks._

object EveryDay {

    def bubbleSort(arr: Array[Int]): Unit = {
        for (i <- 0 until arr.length - 1) {
            for (j <- 0 until arr.length - 1 - i) {
                if (arr(j) > arr(j + 1)) {
                    val temp = arr(j)
                    arr(j) = arr(j + 1)
                    arr(j + 1) = temp
                }
            }
        }
    }

    def mergeSort(arr: Array[Int], left: Int, right: Int, temp: Array[Int]): Unit = {
        if (left < right) {
            val mid: Int = (left + right) / 2
            mergeSort(arr, left, mid, temp)
            mergeSort(arr, mid + 1, right, temp)
            merge(arr, left, mid, right, temp)
        }
    }

    def merge(arr: Array[Int], left: Int, mid: Int, right: Int, temp: Array[Int]): Unit = {
        var i: Int = left
        var j: Int = mid + 1
        var t: Int = 0
        while (i <= mid && j <= right) {
            if (arr(i) <= arr(j)) {
                temp(t) = arr(i)
                t += 1
                i += 1
            } else {
                temp(t) = arr(j)
                t += 1
                j += 1
            }
        }

        while (i <= mid) {
            temp(t) = arr(i)
            t += 1
            i += 1
        }

        while (j <= right) {
            temp(t) = arr(j)
            t += 1
            j += 1
        }

        t = 0

        var tempLeft = left
        while (tempLeft <= right) {
            arr(tempLeft) = temp(t)
            t += 1
            tempLeft += 1
        }
    }

    def quickSort(left: Int, right: Int, arr: Array[Int]): Unit = {
        var l = left
        var r = right
        val pivot = arr((left + right) / 2)
        var temp = 0

        breakable {
            while (l < r) {
                while (arr(l) < pivot)      l += 1
                while (arr(r) > pivot)      r -= 1
                if (l >= r)                 break()

                temp = arr(l)
                arr(l) = arr(r)
                arr(r) = temp

                if (arr(l) == pivot)        r -= 1
                if (arr(r) == pivot)        l += 1
            }
        }
        if (l == r) {
            l += 1
            r -= 1
        }
        if (left < r)                       quickSort(left, r, arr)
        if (right > l)                      quickSort(l, right, arr)
    }

    def binarySearch(arr:Array[Int],l:Int,r:Int,findVal:Int):ArrayBuffer[Int] = {
        if(l > r)   return ArrayBuffer()

        val midIndex = (l + r) / 2
        val midVal = arr(midIndex)

        if(midVal > findVal)        binarySearch(arr, l, midIndex - 1, findVal)
        else if(midVal < findVal)   binarySearch(arr, midIndex + 1, r, findVal)
        else{
            val resArr = ArrayBuffer[Int]()
            var temp = midIndex - 1
            breakable{
                while(true){
                    if(temp < 0 || arr(temp) != findVal)    break()
                    if(arr(temp) == findVal)                resArr.append(temp)
                }
                temp -= 1
            }

            resArr.append(midIndex)

            temp = midIndex + 1
            breakable{
                while(true){
                    if(temp > arr.length - 1 || arr(temp) != findVal)       break()
                    if(arr(temp) == findVal)                                resArr.append(temp)
                    temp += 1
                }
            }
            resArr
        }
    }
}
