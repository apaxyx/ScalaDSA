package sort

import scala.collection.mutable.ArrayBuffer
import scala.util.control.Breaks.{break, breakable}

/**
 * 二分查找
 */
object BinarySearch {

    //TODO 只找到一个值的索引，忽略可能有多个相同值的情况
    def binarySearch(arr: Array[Int], l: Int, r: Int, findVal: Int): Int = {
        //找不到的条件
        if (l > r) return -1

        val midIndex = (l + r) / 2
        val midVal = arr(midIndex)

        if (midVal > findVal) binarySearch(arr, l, midIndex - 1, findVal) //向左进行递归查找
        else if (midVal < findVal) binarySearch(arr, midIndex + 1, r, findVal) //向右进行递归查找
        else midIndex //找到了
    }

    //TODO 找到所有目标值的下标索引
    /**
     * 当首次找到时，因为数组是有序的，所有向这个值的索引的左右开始寻找，当遇到和它相同的时候把索引放入临时数组，
     * 遇到不同的即结束，这个值的首次索引也要加入临时数组，且要对临时数组进行排序
     * */

    def binarySearch2(arr: Array[Int], l: Int, r: Int, findVal: Int): ArrayBuffer[Int] = {
        //找不到的条件
        if (l > r) return ArrayBuffer()


        val midIndex = (l + r) / 2
        val midVal = arr(midIndex)

        if (midVal > findVal) binarySearch2(arr, l, midIndex - 1, findVal) //向左进行递归查找
        else if (midVal < findVal) binarySearch2(arr, midIndex + 1, r, findVal) //向右进行递归查找
        else {
            //定义一个可变数组
            val resArr = ArrayBuffer[Int]()
            //向左扫描
            var temp = midIndex - 1
            breakable {
                while(true){
                    if (temp < 0 || arr(temp) != findVal) {
                        break()
                    }
                    if (arr(temp) == findVal) {
                        resArr.append(temp)
                    }
                    temp -= 1
                }
            }

            //将中间这个索引加入
            resArr.append(midIndex)

            //向右边扫描
            temp = midIndex + 1
            breakable {
                while (true) {
                    if (temp > arr.length - 1 || arr(temp) != findVal) {
                        break()
                    }
                    if (arr(temp) == findVal) {
                        resArr.append(temp)
                    }
                    temp += 1
                }
            }
            resArr
        }

    }

    def test01(): Unit = {
        var arr: Array[Int] = Array(88, 11, 34, 67, 22, 8395, 424).sorted
        println(arr.mkString(","))

        val index: Int = binarySearch(arr, 0, arr.length - 1, 425)

        index match {
            case -1 => println("找不到")
            case _ => println("下标为 " + index + " , 第 " + (index - 1) + " 个数")
        }
    }

    def test02(): Unit = {
        val arr: Array[Int] = Array(88, 11, 34, 67, 22, 22, 22, 22, 22, 22, 22, 22, 8395, 424).sorted

        println(arr.mkString(","))

        val target: ArrayBuffer[Int] = binarySearch2(arr, 0, arr.length - 1, 22)

        target.length match {
            case 0 => println("不存在")
            case _ => println(target.sorted.mkString(","))
        }
    }


    def main(args: Array[String]): Unit = {
        //test01()
        test02()
    }
}
