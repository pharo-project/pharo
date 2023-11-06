Extension { #name : 'Collection' }

{ #category : '*Collections-Sequenceable' }
Collection >> asArray [

	"Answer an Array whose elements are the elements of the receiver.
	Implementation note: Cannot use ''Array withAll: self'' as that only
	works for SequenceableCollections which support the replacement
	primitive."

	"'ab' asArray >>> {$a. $b}"
	"(1 to: 5 by: 3) asArray >>> {1. 4}"

	"#(1 2) asArray == #(1 2) >>> true"

	"'' asArray >>> #()"
	"(10 to: 5) asArray >>> #()"

	| newArray index |
	newArray := Array new: self size.
	index := 0.
	self do: [ :each | newArray at: (index := index + 1) put: each ].
	^ newArray
]

{ #category : '*Collections-Sequenceable' }
Collection >> asNewArray [

	"Like asArray: but return a copy if self is already an Array.
	This ensures that the result is always a new Array"

	"'foo' asNewArray >>> 'foo' asArray"
	"|a| a := #(1 2 3). a asNewArray == a >>> false"

	^ self asArray
]

{ #category : '*Collections-Sequenceable' }
Collection >> asOrderedCollection [

	"Answer an OrderedCollection whose elements are the elements of the
	receiver. The order in which elements are added depends on the order
	in which the receiver enumerates its elements. In the case of unordered
	collections, the ordering is not necessarily the same for multiple
	requests for the conversion."

	"(10 to: 25 by: 5) asOrderedCollection >>> #(10 15 20 25) asOrderedCollection"

	"'foo' asOrderedCollection = #($f $o $o) asOrderedCollection >>> true"
	"'foo' asOrderedCollection = #($o $o $f) asOrderedCollection >>> false"

	^ self as: OrderedCollection
]

{ #category : '*Collections-Sequenceable' }
Collection >> asOrderedDictionary [

	"Answers a Dictionary based on collection of Associations."

	"{'one' -> 1. 'two' ->2} asOrderedDictionary keys first >>> 'one'"

	^ self as: OrderedDictionary
]

{ #category : '*Collections-Sequenceable' }
Collection >> asSortedCollection [

	"Answer a SortedCollection whose elements are the elements of the receiver. The sort order is the default less than or equal. Note that you should use #sorted if you don't really need a SortedCollection, but a sorted collection."

	"'bar' asSortedCollection asArray >>> {$a. $b. $r}."

	"('bar' asSortedCollection add: $c; yourself) asArray >>> {$a. $b. $c. $r}."

	^ self as: SortedCollection
]

{ #category : '*Collections-Sequenceable' }
Collection >> asSortedCollection: aSortBlock [

	"Answer a SortedCollection whose elements are the elements of the receiver. The sort order is defined by the argument, aSortBlock. Note that it is better to use #sorted if you don't really need a SortedCollection, but a sorted collection!!"

	"('bar' asSortedCollection: [:x :y| x>y ]) asArray >>> {$r. $b. $a}."

	| aSortedCollection |
	aSortedCollection := SortedCollection new: self size.
	aSortedCollection sortBlock: aSortBlock.
	aSortedCollection addAll: self.
	^ aSortedCollection
]

{ #category : '*Collections-Sequenceable' }
Collection >> flatCollect: aBlock as: aCollectionClass [
	"Evaluate aBlock for each of the receiver's elements and answer the
	list of all resulting values flatten one level. Assumes that aBlock returns some kind
	of collection for each element. Equivalent to the lisp's mapcan"

	"(#(1 2 3) flatCollect: [:each | { each. each+1 } ] as: Set) >>> #(1 2 3 4) asSet"
	"(#(65 66 67) flatCollect: [:each | { each asCharacter. each asCharacter asLowercase } ] as: String) >>> 'AaBbCc'"

	| col |
	col := OrderedCollection new: self size.
	self do: [ :each | col addAll: (aBlock value: each) ].
	^ aCollectionClass withAll: col
]

{ #category : '*Collections-Sequenceable' }
Collection >> flattened [

	"Flattens a collection of collections (no matter how many levels of collections exist). Strings are considered atoms and, as such, won't be flattened"

	"( #(1 #(2 3) #(4 #(5))) flattened ) >>> #(1 2 3 4 5)"

	"( #('string1' #('string2' 'string3')) flattened ) >>> #('string1' 'string2' 'string3')"

	^ Array streamContents: [ :stream | self flattenOn: stream ]
]

{ #category : '*Collections-Sequenceable' }
Collection >> groupedBy: aBlock [
	"Answer a dictionary whose keys are the result of evaluating aBlock for all my elements, and the value for each key is the selection of my elements that evaluated to that key. Uses species."

	"(#(1 2 3 4 5) groupedBy: [ :v | v odd ]) asString
		>>> 'an OrderedDictionary(true->#(1 3 5) false->#(2 4))'"

	| groups |
	groups := OrderedDictionary new.
	self do: [ :each |
		(groups at: (aBlock value: each) ifAbsentPut: [ OrderedCollection new ]) add: each ].
	self species ~~ OrderedCollection ifTrue: [
		groups associationsDo: [ :association |
			association value: (self species withAll: association value) ]].
	^ groups
]

{ #category : '*Collections-Sequenceable' }
Collection >> sorted [
	"Return a new sequenceable collection which contains the same elements as self but its
elements are sorted"

	"#(3 1 4 2) sorted >>> #(1 2 3 4)"
	"'hello' sorted >>> 'ehllo'"
	"(10 to: 1 by: -2) sorted >>> (2 to: 10 by: 2)"

	^self asArray sorted
]

{ #category : '*Collections-Sequenceable' }
Collection >> sorted: aSortBlockOrNil [
	"Return a new sequenceable collection which contains the same elements as self but its
elements are sorted by aSortBlockOrNil. The block should take two arguments and return true if
the first element should preceed the second one. If aSortBlock is nil then <= is used for
comparison."

	"(#(3 1 4 2) sorted: [:a :b| a>=b]) >>> #(4 3 2 1)"
	"('hello' sorted: [:a :b| a>=b]) >>> 'ollhe'"
	"((1 to: 10 by: 2) sorted: [:a :b| a>=b]) >>> #(9 7 5 3 1)"

	^self asArray sort: aSortBlockOrNil
]
