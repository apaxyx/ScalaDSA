package tree

object BinaryTree {
    def main(args: Array[String]): Unit = {
        val root = new Node(1, "宋江")
        val node2 = new Node(2, "卢俊义")
        val node3 = new Node(3, "公孙胜")
        val node4 = new Node(4, "吴用")
        val node5 = new Node(5, "关胜")

        root.left = node2
        root.right = node3

        node3.left = node4
        node3.right = node5

        val binaryTree = new BinaryTree
        binaryTree.root = root

        println("前序遍历")
        binaryTree.preOrder
        println()

        println("中序遍历")
        binaryTree.infixOrder
        println()

        println("后序遍历")
        binaryTree.postOrder
        println()

        binaryTree.preOrderSearch(6) match {
            case n:Node => printf("找到编号 = %d name = %s\n",n.no,n.name)
            case _ => println("没有找到")
        }

        binaryTree.infixOrderSearch(3) match {
            case n:Node => printf("找到编号 = %d name = %s\n",n.no,n.name)
            case _ => println("没有找到")
        }

        binaryTree.postOrderSearch(5) match {
            case n:Node => printf("找到编号 = %d name = %s\n",n.no,n.name)
            case _ => println("没有找到")
        }

    }
}

class Node(hNo: Int, hName: String) {
    val no = hNo
    var name = hName
    var left: Node = null
    var right: Node = null


    /**
     * 未完成的删除功能
     * 删除节点规则
     * 如果删除的节点是叶子节点，则删除该节点
     * 如果删除的节点是非叶子节点，则删除该子树
     * @param no
     */
    //删除节点
    def delNode(no:Int):Unit = {
        //首先比较当前节点的左子节点是否为要删除的节点
        if(this.left != null && this.left.no == no) {
            this.left = null
            return
        }
        //比较当前节点的右子节点是否为要删除的节点
        if(this.right != null && this.right.no == no) {
            this.right = null
            return
        }
        //向左递归删除
        if(this.left != null) {
            this.left.delNode(no)
        }
        //向右递归删除
        if(this.right != null) {
            this.right.delNode(no)
        }
    }
    //前序查找
    def preOrderSearch(no: Int): Node = {
        if (no == this.no)          return this
        var resNode: Node = null
        if (this.left != null)      resNode = this.left.preOrderSearch(no)
        if (resNode != null)        return resNode
        if (this.right != null)     resNode = this.right.preOrderSearch(no)
        resNode
    }

    //中序查找
    def infixOrderSearch(no: Int): Node = {
        var resNode: Node = null
        if (this.left != null)      resNode = this.left.preOrderSearch(no)
        if (resNode != null)        return resNode
        if (no == this.no)          return this
        if (this.right != null)     resNode = this.right.preOrderSearch(no)
        resNode
    }

    //后序查找
    def postOrderSearch(no: Int): Node = {
        var resNode: Node = null
        if (this.left != null)      resNode = this.left.preOrderSearch(no)
        if (resNode != null)        return resNode
        if (this.right != null)     resNode = this.right.preOrderSearch(no)
        if (resNode != null)        return resNode
        if (no == this.no)          return this
        resNode
    }

    //前序遍历
    def preOrder: Unit = {
        //先输出当前节点的值
        printf("节点信息 no = %d name = %s\n", no, name)

        //向左递归输出左子树
        if (this.left != null)      this.left.preOrder

        //向右递归输出右子树
        if (this.right != null)     this.right.preOrder
    }

    //中序遍历
    def infixOrder: Unit = {
        //向左递归输出左子树
        if (this.left != null)      this.left.infixOrder

        //先输出当前节点的值
        printf("节点信息 no = %d name = %s\n", no, name)

        //向右递归输出右子树
        if (this.right != null)     this.right.infixOrder
    }

    //后序遍历
    def postOrder: Unit = {
        //向左递归输出左子树
        if (this.left != null)      this.left.postOrder

        //向右递归输出右子树
        if (this.right != null)     this.right.postOrder

        //先输出当前节点的值
        printf("节点信息 no = %d name = %s\n", no, name)
    }
}

class BinaryTree {
    var root: Node = null

    //删除节点
    def delNode(no: Int): Unit ={
        if(root != null) {
            if(root.no == no)       root = null
            else                    root.delNode(no)
        }
    }

    //前序查找
    def preOrderSearch(no: Int): Node = {
        if (root != null)   return root.preOrderSearch(no)
        else                return null
    }

    //中序查找
    def infixOrderSearch(no: Int): Node = {
        if (root != null)   return root.infixOrderSearch(no)
        else                return null
    }

    //后序查找
    def postOrderSearch(no: Int): Node = {
        if (root != null)   return root.postOrderSearch(no)
        else                return null
    }

    //前序遍历
    def preOrder: Unit = {
        if (root != null)   root.preOrder
         else               println("当前二叉树为空，不能遍历")

    }

    //中序遍历
    def infixOrder: Unit = {
        if (root != null)   root.infixOrder
        else                println("当前二叉树为空，不能遍历")

    }

    //后序遍历
    def postOrder: Unit = {
        if (root != null)   root.postOrder
        else                println("当前二叉树为空，不能遍历")
    }
}