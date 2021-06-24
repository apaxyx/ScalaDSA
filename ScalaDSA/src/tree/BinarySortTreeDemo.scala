package tree

object BinarySortTreeDemo {

    def main(args: Array[String]): Unit = {

    }

}

class Node(var value:Int){
    var left: Node = null
    var right: Node = null

    //添加方法
    def add(node: Node): Unit ={
        if(node == null)    return
        if(node.value < this.value){
            if(this.left == null)   this.left = node
            else                    this.left.add(node)
        } else {
            if(this.right == null)  this.right = node
            else                    this.right.add(node)
        }
    }

    //中序遍历
    def infixOrder: Unit ={
        if(this.left != null)   this.left.infixOrder
        printf("节点信息 value = %d\n",value)
        if(this.right != null)  this.right.infixOrder
    }

    //根据值查找某个节点
    def search(value:Int): Node = {
        if(value == this.value)     return this
        else if(value > this.value){
            if(this.right == null)  return null
            this.right.search(value)
        }
        else{
            if(this.left == null)   return null
            this.left.search(value)
        }
    }

    //根据值查找某个节点的父节点
    def searchParent(value: Int): Node = {
        if(
            (this.left != null && this.left.value == value)
            ||
            (this.right != null && this.right.value == value)
        )
            this
        else{
            if(value > this.value && this.right != null)            this.right.searchParent(value)
            else if(value < this.value && this.left != null)        this.left.searchParent(value)
            else                                                    null
        }
    }

}


class BinarySortTree{
    var root: Node = null

    def add(node:Node): Unit ={
        if(root == null)    root = node
        else                this.root.add(node)
    }

    def infixOrder: Unit ={
        if(root != null)    root.infixOrder
        else                println("当前二叉树为空，不能遍历")
    }

    //查找节点
    def search(value: Int): Node = {
        if(root != null)    root.search(value)
        else                null
    }

    //查找父节点的方法
    def searchParent(value: Int): Node = {
        if(root != null)    root.searchParent(value)
        else                null
    }

    //删除某个右子树的最小值的节点，并返回最小值
    def delRightTreeMin(node: Node): Int = {
        var target = node
        //使用while循环找到右子树的最小值
        while(target.left != null)      target = target.left
        val minValue = target.value
        //删除最小值对应的节点
        delNode(minValue)
        minValue
    }

    //删除节点
    //先考虑的是叶子节点
    def delNode(value: Int): Unit = {
        if(root == null)    return

        var targetNode = search(value)
        if(targetNode == null)      return

        var parentNode = searchParent(value)
        //TODO 如果这个节点没有父节点，它是根节点，再判断它有没有子节点，它没有子节点那么就删除它
        if(parentNode == null && root.left == null && root.right == null){
            root = null
            return
        }
        //先考虑是叶子节点
        if(targetNode.left == null && targetNode.right == null) {
            //判断删除的节点是parentNode的左子节点，还是右子节点
            if(parentNode.left != null && parentNode.left.value == value)           parentNode.left = null
            else if(parentNode.right != null && parentNode.right.value == value)    parentNode.right = null
        }
        //删除的节点只有一个子节点的情况
        else if(targetNode.left != null && targetNode.right != null){
            //TODO 先排除有两个子节点的情况
            /**
             删除一个有两个节点的节点的情况，
             把以这个节点为根的子树的右子树中的最小值节点删除掉，
             并所这个最小值节点的值赋给这个节点即可
             */
            targetNode.value = delRightTreeMin(targetNode.right)
        }
        else {//TODO 此时的else情况就是只有一个子节点的节点
            if(targetNode.left != null){//要删除的节点的左子节点不为空
                if(parentNode.left.value == value)      parentNode.left = targetNode.left
                else                                    parentNode.right = targetNode.left
            } else {//判断targetNode 是 parentNode的左还是右
                if(parentNode.left.value == value)      parentNode.left = targetNode.right
                else                                    parentNode.right = targetNode.right
            }
        }
    }
}















