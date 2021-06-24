package everyday

import scala.collection.mutable.ArrayBuffer
import util.control.Breaks._

object Day0624 {

    def bubbleSort(arr: Array[Int]): Unit = {
        var flag = true
        var temp = 0
        breakable {
            for (i <- 0 until arr.length - 1) {
                for (j <- 0 until arr.length - i - 1) {
                    if (arr(j) > arr(j + 1)) {
                        flag = false
                        temp = arr(j)
                        arr(j) = arr(j + 1)
                        arr(j + 1) = temp
                    }
                }
                if (flag) break()
                else flag = true
            }
        }
    }


    def quickSort(list: List[Int]): List[Int] = {
        list match {
            case Nil => Nil
            case List() => List()
            case head :: tail => {
                var (left, right) = tail.partition(_ < head)
                quickSort(left) ::: head :: quickSort(right)
            }
        }
    }

    def merge(arr: Array[Int], left: Int, mid: Int,right: Int, temp: Array[Int]): Unit = {
        var i = left
        var j = mid + 1
        var t = 0

        while(i <= mid && j <= right) {
            if(arr(i) < arr(j)){
                temp(t) = arr(i)
                t += 1
                i += 1
            } else {
                temp(t) = arr(j)
                t += 1
                j += 1
            }
        }

        while(i <= mid) {
            temp(t) = arr(i)
            t += 1
            i += 1
        }

        while(j <= right) {
            temp(t) = arr(j)
            t += 1
            j += 1
        }

        t = 0
        var tempLeft = left
        while(tempLeft <= right){
            arr(tempLeft) = arr(t)
            tempLeft += 1
            t += 1
        }
    }

    def mergeSort(arr:Array[Int],left:Int,right:Int,temp:Array[Int]): Unit ={
        if(left < right){
            var mid = (left + right) / 2
            mergeSort(arr,left,mid,temp)
            mergeSort(arr,mid + 1,right,temp)
            merge(arr,left,mid,right,temp)
        }
    }


    def quickSort(arr:Array[Int],left:Int,right:Int): Unit ={
        var l = left
        var r = right
        val pivot = arr((left + right) / 2)
        var temp = 0

        breakable{
            while(l < r){
                while(arr(l) < pivot)   l += 1
                while(arr(r) > pivot)   r -= 1
                if(l >= r)              break()

                if(arr(l) == pivot)     r -= 1
                if(arr(r) == pivot)     l += 1
            }
            if(l == r){
                l += 1
                r -= 1
            }

            if(left < r)    quickSort(arr,left,r)
            if(l < right)   quickSort(arr,l,right)
        }
    }


    def binarySearch(arr:Array[Int],left:Int,right:Int,findVal:Int): ArrayBuffer[Int] ={
        if(left > right)    ArrayBuffer()
        val mid = (left + right) / 2
        val midVal = arr(mid)

        if(midVal > findVal)        binarySearch(arr,left,mid - 1,findVal)
        else if(midVal < findVal)   binarySearch(arr,mid + 1,right,findVal)
        else{
            var res = ArrayBuffer[Int]()
            var temp = mid - 1
            breakable{
                while(true){
                    if(temp < 0 || arr(temp) != findVal)    break()
                    res.append(temp)
                    temp -= 1
                }
            }

            res.append(mid)

            temp = mid + 1
            breakable{
                while(true){
                    if(temp > arr.length - 1 || arr(temp) != findVal)   break()
                    res.append(temp)
                    temp += 1
                }
            }
            res
        }
    }

    def binarySearch(arr:Array[Int],findVal:Int):ArrayBuffer[Int] = {
        var l = 0
        var r = arr.length - 1

        while(l <= r){
            var mid = (l + r) / 2
            var midVal = arr(mid)

            if(midVal == findVal)  {
                var res = ArrayBuffer[Int]()
                var temp = mid - 1
                breakable{
                    while(true){
                        if(temp < 0 || arr(temp) != findVal)    break()
                        res.append(temp)
                        temp -= 1
                    }
                }

                res.append(mid)

                temp = mid + 1
                breakable{
                    while(true){
                        if(temp > arr.length - 1 || arr(temp) != findVal)   break()
                        res.append(temp)
                        temp += 1
                    }
                }
            }
            else if(midVal > findVal)   r = mid - 1
            else                        l = mid + 1
        }
        ArrayBuffer()
    }

}
