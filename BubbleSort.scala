object BubbleSort {

  def bubbleSort[A <% Ordered[A]](list: List[A]): List[A] = {

    def sort(as: List[A], bs: List[A]): List[A] =
      if (as.isEmpty) bs
      else bubble(as, Nil, bs)

    def bubble(as: List[A], zs: List[A], bs: List[A]): List[A] =
      as match {
        case h :: el :: t => if (h > el) bubble(h :: t, el :: zs, bs) else bubble(el :: t, h :: zs, bs)
        case h :: Nil => sort(zs, h :: bs)
      }
    sort(list, Nil)
  }

  def main(args: Array[String]): Unit = {
    val arr = List(2, 1, 3 ,5, 9)
    println(bubbleSort(arr))
  }

}
