object MergeSort {
  def mergeSort[A <% Ordered[A]](list: List[A]): List[A] = {
    def sort(p: (List[A], List[A])): List[A] =
      p match {
        case (Nil, Nil) => Nil
        case (ha :: Nil, Nil) => ha :: Nil
        case (Nil, hb :: Nil) => hb :: Nil
        case (as, bs) => merge(splitAndSort(as), splitAndSort(bs))
      }

    def split(as: List[A]): (List[A], List[A]) = {
      def go(bs: List[A], fs: List[A], ss: List[A]): (List[A], List[A]) =
        bs match {
          case f :: s :: r => go(r, f :: fs, s :: ss)
          case f :: Nil => (f :: fs, ss)
          case Nil => (fs, ss)
        }
      go(as, Nil, Nil)
    }

    def merge(as: List[A], bs: List[A]): List[A] = {
      def go(cs: List[A], ds: List[A], r: List[A]): List[A] =
        (cs, ds) match {
          case (ha :: ta, hb :: tb) =>
            if (ha < hb) go(ta, ds, ha :: r)
            else go(cs, tb, hb :: r)
          case (ha :: ta, Nil) => go(ta, Nil, ha :: r)
          case (Nil, hb :: tb) => go(Nil, tb, hb :: r)
          case (Nil, Nil) => r
        }
      go(as, bs, Nil).reverse
    }

    def splitAndSort(as: List[A]) = sort(split(as))
    splitAndSort(list)
  }

  def main(args: Array[String]): Unit = {
    val arr = List(2, 1, 3, 5, 9)
    println(mergeSort(arr))
  }



}
