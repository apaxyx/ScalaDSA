package sort

import util.control.Breaks._

object QuickSort {
    def quickSort(left: Int, right: Int, arr: Array[Int]): Unit = {
        var l = left
        var r = right
        val pivot = arr((left + right) / 2)
        var temp = 0
        breakable {
            //while语句的作用就是把比pivot小的数放到左边，比pivot大的数放到右边
            while (l < r) {
                while (arr(l) < pivot) { //从左边找一个比pivot大的值的对应下标
                    l += 1
                }
                while (arr(r) > pivot) { //从右边找一个比pivot小的值的对应下标
                    r -= 1
                }
                if (l >= r) { //说明本次交换结束，退出本次while
                    break()
                }

                temp = arr(l) //交换
                arr(l) = arr(r)
                arr(r) = temp
                if (arr(l) == pivot) { //交换处理后，如果发现arr(l) == pivot 就 r -= 1，提高效率
                    r -= 1
                }
                if (arr(r) == pivot) { //交换处理后，如果发现arr(r) == pivot 就 l += 1，提高效率
                    l += 1
                }
            }
        }
        if (l == r) { //提高效率
            l += 1
            r -= 1
        }
        if (left < r) { //向左递归排序
            quickSort(left, r, arr)
        }
        if (right > l) { //向右递归排序
            quickSort(l, right, arr)
        }
    }
    def main(args: Array[String]): Unit = {
        val arr: Array[Int] = Array[Int](7, 6, 5, 4, 3, 2, 1)
        quickSort(0, arr.length - 1, arr)
        println(arr.mkString(","))
    }
}
