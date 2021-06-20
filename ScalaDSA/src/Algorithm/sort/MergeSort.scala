package Algorithm.sort

/**
 * 合并排序，递归实现，分治思想，先分，再排，再合，合的时候再根据大小选择插入临时数组
 * 需要临时数组，如果用C/C++语言，最后需要释放临时数组指针
 */
object MergeSort {

    def main(args: Array[String]): Unit = {
        //main方法测试
        val arr: Array[Int] = Array[Int](7, 6, 5, 13, 14, 21)
        mergeSort(arr, 0, arr.length - 1, new Array[Int](arr.length))
        println(arr.mkString(","))
    }

    def mergeSort(arr: Array[Int], left: Int, right: Int, temp: Array[Int]): Unit = {
        //递归调用，注意区间条件，是“分”的过程
        if (left < right) {
            val mid = (left + right) / 2
            mergeSort(arr, left, mid, temp)
            mergeSort(arr, mid + 1, right, temp)
            merge(arr, left, mid, right, temp) //这一步是“合”的过程
        }
    }

    //“合”
    def merge(arr: Array[Int], left: Int, mid: Int, right: Int, temp: Array[Int]): Unit = {
        var i = left
        var j = mid + 1
        var t = 0
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

        //若两个分的数组还有元素要考虑把它们拷贝进临时数组
        while (i <= mid) {
            temp(t) = arr(i)
            t += 1
            i += 1
        }

        //下面的也得写，不然排序有问题，自己拷贝时没拷贝到，视频里写了，JavaDSA也写了
        while (j <= right) {
            temp(t) = arr(j)
            t += 1
            j += 1
        }

        t = 0

        //将已经有序的临时数组拷贝回原来数组对应的位置
        var tempLeft = left //从left位置开始拷贝，因为是分治数组的一部分
        while (tempLeft <= right) {
            arr(tempLeft) = temp(t)
            t += 1
            tempLeft += 1
        }
    }
}
