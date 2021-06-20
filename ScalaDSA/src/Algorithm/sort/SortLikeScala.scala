package Algorithm.sort

object SortLikeScala {

    //TODO 冒泡排序
    def sort(list: List[Int]): List[Int] = list match {
        case List() => List()
        case head :: tail => compute(head, sort(tail))
    }

    def compute(data: Int, dataSet: List[Int]): List[Int] = dataSet match {
        case List() => List(data)
        case head :: tail => {
            if (data <= head)   data :: dataSet
            else                head :: compute(data, tail)
        }

    }


    //TODO 归并排序
    def mergedSort[T](less: (T, T) => Boolean)(list: List[T]): List[T] = {

        def merged(xList: List[T], yList: List[T]): List[T] = {
            (xList, yList) match {
                case (Nil, _) => yList
                case (_, Nil) => xList
                case (x :: xTail, y :: yTail) => {
                    if (less(x, y))     x :: merged(xTail, yList)
                    else                y :: merged(xList, yTail)
                }
            }
        }

        val n = list.length / 2
        if (n == 0) list
        else {
            val (x, y) = list splitAt n
            merged(mergedSort(less)(x), mergedSort(less)(y))
        }
    }

    //TODO 快速排序
    def quickSort(list: List[Int]): List[Int] = {
        list match {
            case Nil => Nil         //如果是Nil就返回
            case List() => List()   //如果是空List就返回
            case head :: tail =>{   //模式匹配head是第一个元素，tail是剩下的元素
                val (left, right) = tail.partition(_ < head)//用第一个元素做比较
                quickSort(left) ::: head :: quickSort(right)//三个冒号连接List，两个冒号向list追加元素
            }
        }
    }

    def main(args: Array[String]) {
        //TODO 冒泡排序测试
        val list1 = List(3, 12, 43, 23, 7, 1, 2, 0)
        println("冒泡----->" + sort(list1))

        //TODO 归并排序测试
        val list2 = List(3, 12, 43, 23, 7, 1, 2, 0)
        println("归并----->" + mergedSort((x: Int, y: Int) => x < y)(list2))

        //TODO 快速排序测试
        val list3 = List(3, 12, 43, 23, 7, 1, 2, 0)
        println("快排----->" + quickSort(list3))


    }

}
