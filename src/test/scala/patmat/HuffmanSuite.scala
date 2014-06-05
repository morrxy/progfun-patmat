package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
  trait TestTrees {
    val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
    val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
  }

  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }

  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("contain works") {
    assert(contain(List(('a', 1), ('b', 1)), 'a') === true)
    assert(contain(List(('a', 1), ('b', 1)), 'b') === true)
    assert(contain(List(('a', 1), ('b', 1)), 'c') === false)
  }

  test("update works") {
    val ls = List(('a', 1), ('b', 1), ('c', 1))
    println(update(ls, 'a'))
    println(update(ls, 'b'))
    println(update(ls, 'c'))
  }

  test("times works") {
    val t = times(List('a', 'b', 'a', 'a', 'b', 'c'))
    println(t)
  }

  test("makeOrderedLeafList for some frequency table") {
    println(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))))
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }

  test("singleton works") {
    val t2 = Leaf('a',2)
    val t3 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
    val t4 = Leaf('d', 4)

    assert(singleton(List()) === false)
    assert(singleton(List(t2)) === true)
    assert(singleton(List(t2, t3)) === false)
    assert(singleton(List(t2, t3, t4)) === false)
  }

  test("combine.insert works") {
    val f = makeCodeTree(Leaf('e', 1), Leaf('t', 2))
    val l = List(Leaf('x', 4))
    println("combine: " + insert(f, l))
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    val cb1 = combine(leaflist)
    val cb2 = combine(cb1)
    val cb3 = combine(cb2)
    assert(cb1 === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
    assert(cb2 === List(Fork(Fork(Leaf('e',1), Leaf('t', 2), List('e', 't'), 3), Leaf('x',4), List('e', 't', 'x'), 7)))
    assert(cb3 === List(Fork(Fork(Leaf('e',1), Leaf('t', 2), List('e', 't'), 3), Leaf('x',4), List('e', 't', 'x'), 7)))
  }

  test("until works") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    val codetree = until(singleton, combine)(leaflist)
    assert(codetree === List(Fork(Fork(Leaf('e',1), Leaf('t', 2), List('e', 't'), 3), Leaf('x',4), List('e', 't', 'x'), 7)))
  }

  test("createCodeTree") {
    val t1 = createCodeTree(string2Chars("a"))
    println("t1:" + t1)
    val t2 = createCodeTree(string2Chars("ab"))
    println("t2:" + t2)
    val t3 = createCodeTree(string2Chars("aba"))
    println("t3:" + t3)
    val t4 = createCodeTree(string2Chars("abaacb"))
    println("t4:" + t4)
  }

  test("decode works") {
    val t2 = createCodeTree(string2Chars("ab"))
    val l2 = List(1, 0)
    assert(decode(t2, l2) === List('a', 'b'))

    val t3 = createCodeTree(string2Chars("aba"))
    val l3 = List(1, 0, 1)
    assert(decode(t3, l3) === List('a','b','a'))

    val t4 = createCodeTree(string2Chars("abaacb"))
    val l4 = List(1,0,1,1,1,0,0,0,1)
    assert(decode(t4, l4) === List('a','b','a','a','c','b'))
  }

  test("decodedSecret") {
    println("decodedSecret: " + decodedSecret)
  }

//  test("decode and encode a very short text should be identity") {
//    new TestTrees {
//      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
//    }
//  }
}
