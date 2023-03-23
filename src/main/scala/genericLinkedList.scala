sealed trait ConsInterface[+A]{
  def value : A
  def next : ConsInterface[A]

// insert method is used to insert the elements in the beginning of the list
  def insert[B >: A](elem: B): ConsInterface[B] =
    cons(elem, this)

// delete method is used to delete the element from the list
  def delete[B >: A](elem: B): ConsInterface[B] = this match {
    case Nilcons => Nilcons
    case cons(head, tail) if head == elem => tail
    case cons(head, tail) => cons(head, tail.delete(elem))
  }
// find method is used to search the element from the list
  def search[B >: A](elem: B): Option[ConsInterface[B]] = this match {
    case Nilcons => None
    case cons(head, tail) if head == elem => Some(this)
    case cons(head, tail) => tail.search(elem)
  }
// traverse method is used to traverse the list
  def traverse(): Unit = this match {
    case Nilcons => println("End of list")
    case cons(head, tail) => {
      print(s"$head ")
      tail.traverse()
    }
  }
}
// to string method is used to print the elements in the form of linked list
case class cons[A](value : A , next : ConsInterface[A]) extends ConsInterface[A]{
  override def toString = s"head: $value, next: $next"
}
object Nilcons extends ConsInterface[Nothing] {
  def value = throw new NoSuchElementException("head of empty list")

  def next = throw new UnsupportedOperationException("tail of empty list")

  override def toString: String = "Nilcons"
}



