package sort

import scala.collection.mutable.ArrayBuffer
import util.control.Breaks._
object BinarySearchNoRecursive {
    def binarSearchNoRecursive(arr:Array[Int],findVal:Int):ArrayBuffer[Int] = {
        var l = 0
        var r = arr.length - 1
        while(l <= r){
            var mid = (l + r) / 2
            if(arr(mid) == findVal){
                var res = ArrayBuffer[Int]()
                var temp = mid - 1
                breakable{
                    while(true){
                        if(temp < 0 || arr(temp) != findVal)	break()
                        res.append(temp)
                        temp -= 1
                    }
                }
                res.append(mid)
                temp = mid + 1
                breakable{
                    while(true){
                        if(temp > arr.length - 1 || arr(temp) != findVal)	break()
                        res.append(temp)
                        temp += 1
                    }
                }
                res
            }
            else if(arr(mid) > findVal)		r = mid - 1
            else							l = mid + 1
        }
        new ArrayBuffer()
    }

    def main(args: Array[String]): Unit = {
        println(ArrayBuffer)
        println(ArrayBuffer)
        println(new ArrayBuffer)
        println(Array(1))
        println(new Array[Int](1))
    }
}
