package tree

/**
 * 顺序存储满二叉树
 * 用数组存
 * 节点索引：n
 * 节点左子树索引：2n + 1
 * 节点右子树索引：2n + 2
 */
object ArrayTreeDemo {
    def main(args: Array[String]): Unit = {
        val arr = Array(1, 2, 3, 4, 5, 6, 7)

        val tree: ArrayTree = new ArrayTree(arr)
        tree.preOrder()
    }
}

class ArrayTree(var arr: Array[Int]) {
    def preOrder(): Unit = {
        preOrder(0)
    }

    //前序遍历
    def preOrder(index: Int): Unit = {
        if(arr == null || arr.length == 0)      println("数组为空，不能按照二叉树前序遍历")
        //打印当前节点
        print(arr(index) + " ")
        //打印左子节点
        if((index * 2 + 1) < arr.length)        preOrder(index * 2 + 1)
        //打印右子节点
        if((index * 2 + 2) < arr.length)        preOrder(index * 2 + 2)
    }
}
