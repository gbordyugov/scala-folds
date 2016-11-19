object Folds {

  def foldl[A,B](l: List[A]) (z: B) (f: (B, A) => B): B = {
    def g(x: A, k: B => B)(y: B): B = k(f(y, x))
    l.foldRight((x: B) => x) (g) (z)
  }


  def foldr[A,B](l: List[A]) (z: B) (f: (A, B) => B): B = {
    def g(k: B => B, x: A)(y: B): B = k(f(x, y))
    l.foldLeft((x: B) => x) (g) (z)
  }


  def foldlO[A,B](l: List[A]) (z: B)
                 (f: (B, A) => Option[B]): Option[B] = {
    def g(x: A, k: B => Option[B])(z: B): Option[B] =
      f(z, x).flatMap(k(_))
    l.foldRight((x: B) => Option(x))(g)(z)
  }


  def foldrO[A,B](l: List[A]) (z: B)
                 (f: (A, B) => Option[B]): Option[B] = {
    def g(k: B => Option[B], x: A)(z: B): Option[B] =
      f(x, z).flatMap(k(_))
    l.foldLeft((x: B) => Option(x))(g)(z)
  }
}
