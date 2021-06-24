/*
package everyday

import scala.collection.mutable.ArrayBuffer
import scala.util.control.Breaks.{break, breakable}
object Day0622 {

    def main(args: Array[String]): Unit = {
        /* q01(List(4, 6, 3, 24, 3, 3456, 34, 23, 434, 723, 6, 472, 37, 32))
                 .mkString(",")
                 .foreach(print)*/

        List(1,2,3,4) match {
            case head::tail => {
                println(head)//第一个元素
                println(tail)//剩下的元素
                //partition方法返回一个元组，left中是比head小的元素，right是比head大的元素
                val (left,right) = tail.partition(_ < head)
                println(left)
                println(right)
            }
        }
    }

    def bubbleSort(arr:Array[Int]): Unit ={
        for(i <- 0 until arr.length - 1) {
            for(j <- 0 until arr.length - 1 - i) {
                val temp = arr(j)
                arr(j) = arr(j + 1)
                arr(j + 1) = temp
            }
        }
    }


    def quickSort(left:Int,right:Int,arr:Array[Int]): Unit = {
        var l = left
        var r = right
        val pivot = arr((left + right) / 2)
        var temp = 0

        breakable{
            while(l < r){
                while(arr(l) < pivot)   l += 1
                while(arr(r) > pivot)   r -= 1
                if(l >= r)              break()

                temp = arr(l)
                arr(l) = arr(r)
                arr(r) = temp

                if(arr(l) == pivot)     r -= 1
                if(arr(r) == pivot)     l += 1
            }
        }
        if(l == r){
            l += 1
            r -= 1
        }
        if(left < r)        quickSort(left,r,arr)
        if(right > l)       quickSort(l,right,arr)
    }

    def binarySearch(arr:Array[Int],l:Int,r:Int,findVal:Int):ArrayBuffer[Int] = {
        if(l > r)   return ArrayBuffer()

        val midIndex = (l + r) / 2
        val midVal = arr(midIndex)

        if(midVal > findVal)        binarySearch(arr,l,midIndex - 1,findVal)
        else if(midVal < findVal)   binarySearch(arr,midIndex + 1,r,findVal)
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
                    if(temp > arr.length - 1 || arr(temp) != findVal)   break()
                    if(arr(temp) == findVal)                            resArr.append(temp)
                    temp += 1
                }
            }
            resArr
        }
    }

    def bubble01(arr:Array[Int]): Unit ={
        for(i <- 0 until arr.length - 1)
            for(j <- 0 until arr.length - 1 - i)
                if(arr(j) > arr(j + 1)) {
                    val temp = arr(j)
                    arr(j) = arr(j + 1)
                    arr(j + 1) = temp
                }
    }

    def buuble02(arr:Array[Int]): Unit ={
        for(i <- 0 until arr.length - 1)
            for(j <- 0 until arr.length - i - 1)
                if(arr(j) < arr(j + 1)){
                    val temp = arr(j)
                    arr(j) = arr(j + 1)
                    arr(j + 1) = temp
                }
    }

    def bubble03(arr:Array[Int]): Unit ={
        for(i <- 0 until arr.length - 1)
            for(j <- 0 until arr.length - i - 1)
                if(arr(j) < arr(j + 1)){
                    val temp = arr(j)
                    arr(j) = arr(j + 1)
                    arr(j + 1) = arr(j)
                }
    }

    def bubble04(arr:Array[Int]): Unit ={
        var flag = true
        var temp = 0
        breakable{
            for(i <- 0 until arr.length - 1) {
                for (j <- 0 until arr.length - i - 1) {
                    if (arr(j) < arr(j + 1)) {
                        flag = false
                        temp = arr(j)
                        arr(j) = arr(j + 1)
                        arr(j + 1) = temp
                    }
                }

                if(flag)    break()
                else        flag = true;
            }
        }
    }

    def bubble05(arr:Array[Int]): Unit ={
        var flag = true;
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
                if(flag)    break()
                else        flag = true;
            }
        }
    }

    def bublle06(arr:Array[Int]): Unit ={
        var flag = true;
        var temp = 0
        breakable{
            for(i <- 0 until arr.length - 1){
                for(j <- 0 until arr.length - 1 - i){
                    if(arr(j) > arr(j + 1)){
                        flag = false
                        temp = arr(j)
                        arr(j) = arr(j + 1)
                        arr(j + 1) = temp
                    }
                }
                if(flag)    break()
                else        flag = true;
            }
        }
    }

    def q01(list:List[Int]):List[Int] = {
        list match {
            case Nil => Nil
            case List() => List()
            case head::tail =>{
                val (left,right) = tail.partition(_ < head)
                q01(left) ::: head :: q01(right)
            }
        }
    }

    def q02(list:List[Int]):List[Int]= {
        list match{
            case Nil => Nil
            case List() => List()
            case head::tail => {
                val (left,right) = tail.partition(_ < head)
                q02(left) ::: head :: q02(right)
            }

        }
    }

    def q03(list:List[Int]):List[Int] ={
        list match {
            case Nil => Nil//Nil是一个空List,定义为List[Nothing]
            case List() => List()//没有元素的集合
            case head :: tail => {
                val (left,right) = tail.partition(_ < head)
                q03(left) ::: head :: q03(right)//:::是连接两个List,::是连接一个值和一个List,List在右边
            }
        }
    }

    def q04(list:List[Int]):List[Int] = {
        list match {
            case Nil => Nil
            case List() => List()
            case head :: tail => {
                val (left, right) = tail.partition(_ < head)
                q04(left) ::: head :: q04(right)
            }
        }
    }

    def bubble07(arr:Array[Int]): Unit ={
        var flag = true;
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
                if(flag)    break()
                else        flag = true
            }
        }
    }

    def q05(list:List[Int]):List[Int]={
        list match{
            case Nil => Nil
            case List() => List()
            case head::tail =>{
                val (left,right) = tail.partition(_ < head)
                q05(left) ::: head :: q05(right)
            }
        }
    }



    def merge01: Unit ={

        def mergeSort(arr:Array[Int],left:Int,right:Int,temp:Array[Int]): Unit ={
            if(left < right){
                val mid = (left + right) / 2
                mergeSort(arr,left,mid,temp)
                mergeSort(arr,mid + 1,right,temp)
                merge(arr,left,mid,right,temp)
            }
        }

        def merge(arr:Array[Int],left:Int,mid:Int,right:Int,temp:Array[Int]): Unit ={
            var i = left
            var j = mid + 1
            var t = 0
            while(i < mid && j <= right){
                if(arr(i) <= arr(j)){
                    temp(t) = arr(i)
                    t += 1
                    i += 1
                } else{
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
                t += 1
                tempLeft += 1
            }
        }
    }


    def merge02: Unit ={
        def merge(arr:Array[Int],left:Int,mid:Int,right:Int,temp:Array[Int]): Unit ={
            var i = left
            var j = mid + 1
            var t = 0
            while(i <= mid && j <= right){
                if(arr(i) <= arr(j)) {
                    temp(t) = arr(i)
                    t += 1
                    i += 1
                } else{
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
                t += 1
                tempLeft += 1
            }
        }

        def mergeSort(arr:Array[Int],left:Int,right:Int,temp:Array[Int]): Unit ={
            if(left < right){
                val mid = (left + right) / 2
                mergeSort(arr,left,mid,temp)
                mergeSort(arr,mid + 1,right,temp)
                merge(arr,left,mid,right,temp)
            }
        }
    }

    def merge03: Unit ={
        def merge(arr:Array[Int],left:Int,mid:Int,right:Int,temp:Array[Int]): Unit ={
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
                t += 1
                tempLeft += 1
            }
        }

        def mergeSort(arr:Array[Int],left:Int,right:Int,temp:Array[Int]): Unit ={
            val mid = (left + right) / 2
            mergeSort(arr,left,mid,temp)
            mergeSort(arr,mid + 1,right,temp)
            merge(arr,left,mid,right,temp)
        }
    }

    def bubbleSort08(arr:Array[Int]): Unit ={
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
                if(flag)    break
                else        flag = true
            }
        }
    }

    def fastSort(list:List[Int]):List[Int] = {
        list match{
            case Nil => Nil
            case List() => List()
            case head::tail => {
                val (left,right) = tail.partition(_ < head)
                fastSort(left) ::: head :: fastSort(right)
            }
        }
    }



    def merge03: Unit ={

        def merge(arr: Array[Int], left: Int, mid: Int, right: Int, temp: Array[Int]) ={
            var i = left
            var j = mid + 1
            var t = 0
            while(i <= mid && j <= right){
                if(arr(i) <= arr(j)){
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
                temp(t) = arr(i)
                t += 1
                j += 1
            }
            t = 0
            var tempLeft = left
            while(tempLeft< right){
                arr(tempLeft) = temp(t)
                tempLeft += 1
                t += 1
            }
        }

        def mergeSort(arr:Array[Int],left:Int,right:Int,temp:Array[Int]): Unit = {
            if(left < right){
                val mid = (left + right) / 2
                mergeSort(arr,left,mid,temp)
                mergeSort(arr,mid + 1,right,temp)
                merge(arr,left,mid,right,temp)
            }
        }
    }

    def qs01(left:Int,right:Int,arr:Array[Int]): Unit ={
        var l = left
        var r = right
        val pivot = arr((left + right) / 2)
        var temp = 0

        breakable{
            while(l < r){
                while(arr(l) < pivot)   l += 1
                while(arr(r) > pivot)   r -= 1
                if(l >= r)              break()

                temp = arr(l)
                arr(l) = arr(r)
                arr(r) = temp

                if(arr(l) == pivot)     r -= 1
                if(arr(r) == pivot)     l += 1
            }
        }

        if(l == r){
            l += 1
            r -= 1
        }
        if(left < r)        qs01(left,r,arr)
        if(right > l)       qs01(l,right,arr)
    }

    def qs02(left:Int,right:Int,arr:Array[Int]): Unit = {
        var l = left
        var r = right
        val pivot = arr((left + right) / 2)
        var temp = 0

        breakable{
            while(l < r){
                while(arr(l) < pivot)   l += 1
                while(arr(r) > pivot)   r -= 1
                if(l >= r)              break()

                temp = arr(l)
                arr(l) = arr(r)
                arr(r) = temp

                if(arr(l) == pivot)     r -= 1
                if(arr(r) == pivot)     l += 1
            }
        }

        if(l == r){
            l += 1
            r -= 1
        }
        if(left < r)        qs02(left,r,arr)
        if(right > l)       qs02(l,right,arr)
    }

    def qs03(left:Int,right:Int,arr:Array[Int]): Unit ={
        var l = left
        var r = right
        var pivot = arr((left + right) / 2)
        var temp = 0

        breakable{
            while(l < r){
                while(arr(l) < pivot)   l += 1
                while(arr(r) > pivot)   r -= 1
                if(l > r)               break()

                temp = arr(l)
                arr(l) = arr(r)
                arr(r) = temp

                if(arr(l) == pivot)     r -= 1
                if(arr(r) == pivot)     l += 1
            }
        }

        if(l == r){
            l += 1
            r -= 1
        }
        if(left < r)        qs03(left,r,arr)
        if(left > l)        qs03(l,right,arr)
    }


}

*/
