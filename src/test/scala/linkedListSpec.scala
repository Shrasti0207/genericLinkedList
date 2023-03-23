import org.scalatest.funsuite.AnyFunSuite

class linkedListSpec extends  AnyFunSuite{
  test("A LinkedList should be empty when initialized with Nilcons" ) {
    val list = Nilcons
    assert(list.isInstanceOf[ConsInterface[Nothing]])
  }

  test("Insert the elements in to a list"){
    val list = cons(1, Nilcons).insert(2).insert(3)
    assert(list.toString === "head: 3, next: head: 2, next: head: 1, next: Nilcons")
  }


  test("Deleting an element that exists in the list") {
    val list = cons(1, cons(2, Nilcons))
    val list1 = list.delete(2)
    assert(list1.toString == "head: 1, next: Nilcons")
  }


  test("find elements correctly"){
    val list = cons(1, Nilcons).insert(2).insert(3)
    assert(list.search(2).get.toString === "head: 2, next: head: 1, next: Nilcons")
  }


  test("return None when finding a non-existing element") {
    val list = cons(1, Nilcons).insert(2).insert(3)
    assert(list.search(4) === None)
  }

  test("A LinkedList traverse the elements and print them"){
    val list = cons(1, Nilcons).insert(2).insert(3)

    val outputStream = new java.io.ByteArrayOutputStream()
    Console.withOut(outputStream) {
      list.traverse()
    }
    val expectedOutput = "3 2 1 End of list\n"
    assert(outputStream.toString == (expectedOutput))
  }

}
