package everyday

import scala.collection.mutable.ArrayBuffer
import util.control.Breaks._

object Day0623 {

    def bubble(arr: Array[Int]): Unit = {
        var flag = true
        var temp = 0
        breakable{
            for(i <- 0 until arr.length - 1){
                for(j <- 0 until arr.length - i - 1){
                    if(arr(j) > arr(j + 1)){
                        flag = false
                        temp = arr(j)
                        arr(j) = arr(j + 1)
                        arr(j + 1) = temp
                    }
                }
                if(flag)        break()
                else            flag = true
            }
        }
    }



    def merge(arr: Array[Int], left: Int, mid: Int, right: Int, temp: Array[Int]): Unit = {
        var i = left
        var j = mid + 1
        var t = 0

        while(i <= mid && j <= right){
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

        while(i <= mid){
            temp(t) = arr(i)
            t += 1
            i += 1
        }

        while(j <= right){
            temp(t) = arr(j)
            t += 1
            j += 1
        }

        t = 0
        var tempLeft = left
        while(tempLeft <= right){
            arr(tempLeft) = temp(t)
            tempLeft += 1
            t += 1
        }
    }

    def mergeSort(arr: Array[Int], left: Int, right: Int, temp: Array[Int]): Unit = {
        if(left < right){
            var mid = (left + right) / 2
            mergeSort(arr,left,mid,temp)
            mergeSort(arr,mid + 1,right,temp)
            merge(arr,left,mid,right,temp)
        }
    }

    def fast(list: List[Int]): List[Int] = {
        list match{
            case Nil => Nil
            case List() => List()
            case head :: tail =>{
                val (left,right) = tail.partition(_ < head)
                fast(left) ::: head :: fast(right)
            }
        }
    }


    def quickSort(left: Int, right: Int, arr: Array[Int]): Unit = {
        var l = left
        var r = right
        var pivot = arr((left + right) / 2)
        var temp = 0
        breakable{
            while(l < r){
                while(arr(l) < pivot)   l += 1
                while(arr(r) > pivot)   r -= 1
                if(l >= r)              break()

                temp = arr(l)
                arr(l) = arr(r)
                arr(r) = arr(l)

                if(arr(l) == pivot)     r -= 1
                if(arr(r) == pivot)     l += 1
            }

            if(l == r){
                l += 1
                r -= 1
            }
            if(left < r)    quickSort(left,r,arr)
            if(l < right)   quickSort(l,right,arr)
        }

    }

    def binarySearch(arr: Array[Int], l: Int, r: Int, findVal: Int): ArrayBuffer[Int] = {
        if(l > r)   return ArrayBuffer[Int]()

        var mid = (l + r) / 2
        var midVal = arr(mid)

        if(findVal < midVal)        binarySearch(arr,l,mid - 1,findVal)
        else if(findVal > midVal)  binarySearch(arr,mid + 1,r,findVal)
        else{
            val res = new ArrayBuffer[Int]()

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





    def main(args: Array[String]): Unit = {
        val arr: Array[Int] = Array(10, 9, 9, 9, 9, 9, 7, 8)
        quickSort(0, arr.length - 1, arr)
        println(arr.mkString(","))

        //val buf: ArrayBuffer[Int] = binarySearch(arr, 0, arr.length - 1, 9)
        val buf2: ArrayBuffer[Int] = binarySearch(arr, 0, arr.length - 1, 9)
        //println(buf.mkString(","))
        println(buf2.mkString(","))

    }

}
