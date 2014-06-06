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

//  test("treeContain works") {
//    val t1 = createCodeTree(string2Chars("a"))
//    val t4 = createCodeTree(string2Chars("abaacb"))
//
//    assert(treeContain('a', t1) === true)
//    assert(treeContain('b', t1) === false)
//
//    assert(treeContain('a', t4) === true)
//    assert(treeContain('b', t4) === true)
//    assert(treeContain('c', t4) === true)
//    assert(treeContain('d', t4) === false)
//
//    assert(treeContain('h', frenchCode) === true)
//    assert(treeContain('c', frenchCode) === true)
//    assert(treeContain('1', frenchCode) === false)
//    assert(treeContain('2', frenchCode) === false)
//  }

  test("encode works") {
    val l1 = string2Chars("a")
    val t1 = createCodeTree(l1)
    println("l1:" + l1)
    println("t1:" + t1)
    println("encode t1:" + encode(t1)(l1))

    val l2 = string2Chars("ab")
    val t2 = createCodeTree(l2)
    println("l2:" + l2)
    println("t2:" + t2)
    println("encode t2:" + encode(t2)(l2))

    val l3 = string2Chars("aba")
    val t3 = createCodeTree(l2)
    println("l3:" + l3)
    println("t3:" + t3)
    println("encode t3:" + encode(t3)(l3))
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("codeBits works") {
    val table = List(('a', List(0, 1)), ('b', List(1)), ('c', List(0, 1, 1)))
    assert(codeBits(table)('a') == List(0, 1))
    assert(codeBits(table)('b') == List(1))
    assert(codeBits(table)('c') == List(0, 1, 1))
  }

  test("mergeCodeTables works") {
    val t0 = List()
    val t2 = List(('a', List(1)), ('b', List(0, 1)))
    val t3 = List(('c', List(1, 0, 1)), ('d', List(1, 1, 1)), ('e', List(0, 1, 0)))

    assert(mergeCodeTables(t0, t2) === List(('a', List(1)), ('b', List(0, 1))))
    assert(mergeCodeTables(t2, t3) === List(('a', List(1)), ('b', List(0, 1)), ('c', List(1, 0, 1)), ('d', List(1, 1, 1)), ('e', List(0, 1, 0))))
    assert(mergeCodeTables(t3, t2) === List(('c', List(1, 0, 1)), ('d', List(1, 1, 1)), ('e', List(0, 1, 0)), ('a', List(1)), ('b', List(0, 1))))
  }

  test("convert works") {
    val t0 = Leaf('a', 2)
    val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
    val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
    assert(convert(t0) === List(('a', List())))
    assert(convert(t1) === List(('a', List(0)), ('b', List(1))))
    assert(convert(t2) === List(('a', List(0,0)), ('b', List(0,1)), ('d', List(1))))
  }

  test("putolast works") {
    val l1 = List(0)
    val l2 = List(1, 0)
    val l3 = List(0, 1, 0)
    assert(putToLast(0, l1) === List(0, 0))
    assert(putToLast(1, l1) === List(0, 1))
    assert(putToLast(0, l2) === List(1, 0, 0))
    assert(putToLast(1, l2) === List(1, 0, 1))
    assert(putToLast(0, l3) === List(0, 1, 0, 0))
    assert(putToLast(1, l3) === List(0, 1, 0, 1))
  }

  test("quickEncode works") {
    val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
    val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)

    val b0 = quickEncode(t1)(List())
    val b1 = quickEncode(t1)(List('a'))
    val b2 = quickEncode(t1)(List('a','b'))

    assert(quickEncode(t1)(List()) === List())
    assert(quickEncode(t1)(List('a')) === List(0))
    assert(quickEncode(t1)(List('b')) === List(1))
    assert(quickEncode(t1)(List('b','a')) === List(1, 0))
    assert(quickEncode(t2)(List('a','b','d')) === List(0,0,0,1,1))
    assert(quickEncode(t2)(List('d','a','b','b','a')) === List(1,0,0,0,1,0,1,0,0))

  }

}
