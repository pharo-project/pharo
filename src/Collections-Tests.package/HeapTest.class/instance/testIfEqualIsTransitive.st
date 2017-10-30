tests
testIfEqualIsTransitive
	"This is http://bugs.squeak.org/view.php?id=6943"

    | anArray heap1 heap2 |
    anArray := #(1 2 3).
    heap1 := Heap withAll: (1 to: 3) sortBlock: [:a :b | a < b].
    heap2 := Heap withAll: (1 to: 3) sortBlock: [:a :b | b > a].
    self
		assert: (heap1 = anArray) & (heap2 = anArray) ==> (heap1 = heap2)
		description: 'Heap equality should be transitive'