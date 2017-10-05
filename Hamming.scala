object Hamming {
  def distance(s1: String, s2: String): Option[Int] = {
    if (s1.length != s2.length)
      return None
    var acc = 0
    s1.toList.zip(s2.toList).foreach((x) => {
      val (c1, c2) = x
      if (c1 != c2) 
        acc = acc + 1
    })
    Some(acc)
  }
}