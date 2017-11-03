"
SUnit tests for keyed trees
"
Class {
	#name : #KeyedTreeTest,
	#superclass : #TestCase,
	#category : #Collections-Tests-Unordered
}

{ #category : #tests }
KeyedTreeTest >> testAtPath [
	"Test the at path method for success and failure modes."

	|tree t2|
	tree := KeyedTree new
		at: 1 put: (t2 := KeyedTree new
					at: #two put: 'One-Two';
					at: #three put: 'One-Three';
					yourself);
		at: 2 put: 'Two';
		yourself.
	self should: [(tree atPath: #(1)) = t2].
	self should: [(tree atPath: #(1 two)) = 'One-Two'].
	self should: [(tree atPath: #(1 three)) = 'One-Three'].
	self should: [(tree atPath: #(2)) = 'Two'].
	self should: [tree atPath: #(2 4)] raise: self defaultTestError.
	self should: [tree atPath: #(1 two three)] raise: self defaultTestError.
	self should: [tree atPath: #(3)] raise: self defaultTestError.
]

{ #category : #tests }
KeyedTreeTest >> testAtPathIfAbsent [
	"Test the at path if absent method for success and failure modes."

	|tree t2|
	tree := KeyedTree new
		at: 1 put: (t2 := KeyedTree new
					at: #two put: 'One-Two';
					at: #three put: 'One-Three';
					yourself);
		at: 2 put: 'Two';
		yourself.
	self should: [(tree atPath: #(1) ifAbsent: []) = t2].
	self should: [(tree atPath: #(1 two) ifAbsent: []) = 'One-Two'].
	self should: [(tree atPath: #(1 three) ifAbsent: []) = 'One-Three'].
	self should: [(tree atPath: #(2) ifAbsent: []) = 'Two'].
	self should: [(tree atPath: #(2 4) ifAbsent: [#missing]) = #missing].
	self should: [(tree atPath: #(1 two three) ifAbsent: [#missing]) = #missing] raise: self defaultTestError.
	self should: [(tree atPath: #(3) ifAbsent: [#missing]) = #missing].
]

{ #category : #tests }
KeyedTreeTest >> testAtPathIfAbsentPut [
	"Test the at path if absent put method for success and failure modes."

	|tree t2|
	tree := KeyedTree new
		at: 1 put: (t2 := KeyedTree new
					at: #two put: 'One-Two';
					at: #three put: 'One-Three';
					yourself);
		at: 2 put: 'Two';
		yourself.
	self should: [(tree atPath: #(1) ifAbsentPut: [#new]) = t2].
	self should: [(tree atPath: #(1 two) ifAbsentPut: [#new]) = 'One-Two'].
	self should: [(tree atPath: #(1 three) ifAbsentPut: [#new]) = 'One-Three'].
	self should: [(tree atPath: #(2) ifAbsentPut: [#new]) = 'Two'].
	self should: [tree atPath: #(2 4) ifAbsentPut: [#new]] raise: self defaultTestError.
	self should: [tree atPath: #(1 two three) ifAbsentPut: [#new]] raise: self defaultTestError.
	self should: [(tree atPath: #(1 four one) ifAbsentPut: [#anotherNew]) = #anotherNew].
	self should: [(tree atPath: #(1 four one)) = #anotherNew].
	self should: [(tree atPath: #(3) ifAbsentPut: [#yetAnotherNew]) = #yetAnotherNew].
	self should: [(tree atPath: #(3)) = #yetAnotherNew].
]

{ #category : #tests }
KeyedTreeTest >> testAtPathPut [
	"Test the at path put method for success and failure modes."

	|tree t2|
	tree := KeyedTree new
		at: 1 put: (KeyedTree new
					at: #two put: 'One-Two';
					at: #three put: 'One-Three';
					yourself);
		at: 2 put: 'Two';
		yourself.
	self should: [(tree atPath: #(1 two) put: #new) = #new].
	self should: [(tree atPath: #(1 two)) = #new].
	self should: [(tree atPath: #(1 three) put: (t2 := KeyedTree new)) = t2].
	self should: [(tree atPath: #(1 three $1) put: #anotherNew) = #anotherNew].
	self should: [(tree atPath: #(1 three $1)) = #anotherNew].
	self should: [tree atPath: #(2 4) put: [#new]] raise: self defaultTestError.
	self should: [(tree atPath: #(1 four one) put: #anotherNew) = #anotherNew].
	self should: [(tree atPath: #(1 four one)) = #anotherNew].
]

{ #category : #tests }
KeyedTreeTest >> testCopy [
	"Test the copy method for success and failure modes."

	|c tree t2 t3|
	tree := KeyedTree new
		at: 1 put: (t2 := KeyedTree new
					at: #two put: 'One-Two';
					at: #three put: 'One-Three';
					at: #four put: (t3 := KeyedTree new);
					yourself);
		at: 2 put: 'Two';
		yourself.
	c := tree copy.
	self should: [c = tree].
	self shouldnt: [c == tree].
	self should: [(c at: 1) = t2].
	self shouldnt: [(c at: 1) == t2].
	self should: [(c atPath: #(1 four)) = t3].
	self shouldnt: [(c atPath: #(1 four)) == t3].
]

{ #category : #tests }
KeyedTreeTest >> testMerge [
	"Test the merge method for success and failure modes."

	|t1 t2 m|
	t1 := KeyedTree new
		at: 1 put: (KeyedTree new
					at: 1 put: '1-1';
					at: 2 put: '1-2';
					at: 3 put: (KeyedTree new
								at: 1 put: '1-3-1';
								at: 2 put: '1-3-2';
								yourself);
					yourself);
		at: 2 put: '2';
		yourself.
	t2 := KeyedTree new
		at: 1 put: (KeyedTree new
					at: 1 put: (KeyedTree new
								at: 1 put: '1-1-1';
								at: 2 put: '1-1-2';
								yourself);
					at: 2 put: '1-2*';
					yourself);
		at: 3 put: '3';
		yourself.
	m := t1 merge: t2.
	self should: [(m at: 2) = '2'].
	self should: [(m at: 3) = '3'].
	self should: [(m atPath: #(1 2)) = '1-2*'].
	self should: [(m atPath: #(1 1 1)) = '1-1-1'].
	self should: [(m atPath: #(1 1 2)) = '1-1-2'].
	self should: [(m atPath: #(1 3 1)) = '1-3-1'].
	self should: [(m atPath: #(1 3 2)) = '1-3-2'].
]

{ #category : #tests }
KeyedTreeTest >> testRemovePath [
	"Test the remove path method for success and failure modes."

	|tree|
	tree := KeyedTree new
		at: 1 put: (KeyedTree new
					at: #two put: 'One-Two';
					at: #three put: 'One-Three';
					yourself);
		at: 2 put: 'Two';
		yourself.
	self should: [tree removePath: #(4)] raise: self defaultTestError.
	self should: [tree removePath: #(1 one)] raise: self defaultTestError.
	self should: [(tree removePath: #(1 two)) = 'One-Two'].
	self should: [(tree atPath: #(1 two) ifAbsent: []) = nil].
	self should: [(tree removePath: #(2)) = 'Two'].
	self should: [(tree atPath: #(2) ifAbsent: []) = nil].
]

{ #category : #tests }
KeyedTreeTest >> testRemovePathIfAbsent [
	"Test the remove path if absent method for success and failure modes."

	|tree|
	tree := KeyedTree new
		at: 1 put: (KeyedTree new
					at: #two put: 'One-Two';
					at: #three put: 'One-Three';
					yourself);
		at: 2 put: 'Two';
		yourself.
	self should: [(tree removePath: #(4) ifAbsent: [#none]) = #none].
	self should: [(tree removePath: #(1 2 3 4) ifAbsent: [#none]) = #none].
	self should: [(tree removePath: #(1 two) ifAbsent: [#none]) = 'One-Two'].
	self should: [(tree atPath: #(1 two) ifAbsent: []) = nil].
	self should: [(tree removePath: #(2) ifAbsent: [#none]) = 'Two'].
	self should: [(tree atPath: #(2) ifAbsent: []) = nil].
]

{ #category : #tests }
KeyedTreeTest >> testSubtrees [
	"Test the subtrees method for success and failure modes."

	|t1 t2 t3 t4|
	t1 := KeyedTree new
		at: 1 put: (t2 := KeyedTree new
					at: 1 put: '1-1';
					at: 2 put: '1-2';
					at: 3 put: (t3 := KeyedTree new
								at: 1 put: '1-3-1';
								at: 2 put: '1-3-2';
								yourself);
					yourself);
		at: 2 put: '2';
		at: 3 put: (t4 := KeyedTree new
					at: 1 put: '1-3-1';
					at: 2 put: '1-3-2';
					yourself);
		yourself.
	self should: [t1 subtrees = {t2. t4}].
	self should: [(t1 at: 1) subtrees = {t3}].
]
