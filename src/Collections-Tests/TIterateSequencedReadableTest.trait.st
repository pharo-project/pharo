"
A trait for test purposes
"
Trait {
	#name : #TIterateSequencedReadableTest,
	#category : #'Collections-Tests-Abstract'
}

{ #category : #requirements }
TIterateSequencedReadableTest >> empty [
	^self explicitRequirement.
]

{ #category : #requirements }
TIterateSequencedReadableTest >> nonEmptyMoreThan1Element [
" return a collection that doesn't includes equal elements' and doesn't include nil elements'" 
	^self explicitRequirement.
]

{ #category : #'tests - fixture' }
TIterateSequencedReadableTest >> test0FixtureIterateSequencedReadableTest [
	| res |
	self nonEmptyMoreThan1Element.
	self assert: self nonEmptyMoreThan1Element size > 1.

	self empty.
	self assertEmpty: self empty.

	res := true.
	self nonEmptyMoreThan1Element detect: [ :each | (self nonEmptyMoreThan1Element occurrencesOf: each) > 1 ] ifNone: [ res := false ].
	self deny: res
]

{ #category : #'tests - iterate on sequenced reable collections' }
TIterateSequencedReadableTest >> testAllButFirstDo [
	
	| result |
	result:= OrderedCollection  new.
	
	self nonEmptyMoreThan1Element  allButFirstDo: [:each | result add: each].
	
	1 to: (result size) do:
		[:i|
		self assert: (self nonEmptyMoreThan1Element  at:(i +1))=(result at:i)].
	
	self assert: result size=(self nonEmptyMoreThan1Element size-1)
]

{ #category : #'tests - iterate on sequenced reable collections' }
TIterateSequencedReadableTest >> testAllButLastDo [
	
	| result |
	result:= OrderedCollection  new.
	
	self nonEmptyMoreThan1Element  allButLastDo: [:each | result add: each].
	
	1 to: (result size) do:
		[:i|
		self assert: (self nonEmptyMoreThan1Element  at:(i ))=(result at:i)].
	
	self assert: result size=(self nonEmptyMoreThan1Element  size-1)
]

{ #category : #'tests - iterate on sequenced reable collections' }
TIterateSequencedReadableTest >> testCollectFromTo [
	
	| result |
	result:=self nonEmptyMoreThan1Element 
		collect: [ :each | each ]
		from: 1
		to: (self nonEmptyMoreThan1Element size - 1).
		
	1 to: result size
		do: [ :i | self assert: (self nonEmptyMoreThan1Element at: i) = (result at: i) ].
	self assert: result size = (self nonEmptyMoreThan1Element size - 1)
]

{ #category : #'tests - iterate on sequenced reable collections' }
TIterateSequencedReadableTest >> testDetectSequenced [
	"Testing that detect keep the first element returning true for sequenceable collections"

	| element result |
	element := self nonEmptyMoreThan1Element at: 1.
	result := self nonEmptyMoreThan1Element  detect: [:each | each notNil ].
	self assert: result equals: element 
]

{ #category : #'tests - iterate on sequenced reable collections' }
TIterateSequencedReadableTest >> testDo [
]

{ #category : #'tests - iterate on sequenced reable collections' }
TIterateSequencedReadableTest >> testFindFirst [

	| element result |
	element := self nonEmptyMoreThan1Element at: 1.
	result := self nonEmptyMoreThan1Element findFirst: [:each | each = element].
	
	self assert: result equals: 1 
]

{ #category : #'tests - iterate on sequenced reable collections' }
TIterateSequencedReadableTest >> testFindFirstNotIn [

	| result |	
	result := self empty findFirst: [:each | true ].
	
	self assert: result equals: 0
]

{ #category : #'tests - iterate on sequenced reable collections' }
TIterateSequencedReadableTest >> testFindLast [

	| element result |
	element := self nonEmptyMoreThan1Element at: self nonEmptyMoreThan1Element size.
	result := self nonEmptyMoreThan1Element findLast: [:each | each =element].
	
	self assert: result equals: self nonEmptyMoreThan1Element size
]

{ #category : #'tests - iterate on sequenced reable collections' }
TIterateSequencedReadableTest >> testFindLastNotIn [

	| result |	
	result := self empty findFirst: [:each | true ].
	
	self assert: result equals: 0
]

{ #category : #'tests - iterate on sequenced reable collections' }
TIterateSequencedReadableTest >> testFromToDo [
	
	| result |
	result:= OrderedCollection new.
	
	self nonEmptyMoreThan1Element from: 1 to: (self nonEmptyMoreThan1Element size -1) do: [:each | result add: each].
	
	1 to: (self nonEmptyMoreThan1Element  size -1) do:
		[:i | self assert: (self nonEmptyMoreThan1Element at:i) = (result at:i) ].

	self assert: result size = (self nonEmptyMoreThan1Element size - 1).
]

{ #category : #'tests - iterate on sequenced reable collections' }
TIterateSequencedReadableTest >> testGroupsOfAtATimeCollect [
	| groupsOf2 |
	self assertEmpty: (self empty groupsOf: 2 atATimeCollect: [ :x | x ]).
	self assertEmpty: (self empty groupsOf: 2 atATimeCollect: #yourself).
	
	groupsOf2 := self nonEmptyMoreThan1Element groupsOf: 2 atATimeCollect: [ :x | x ].
	
	self assert: groupsOf2 size equals: self nonEmptyMoreThan1Element size // 2.
	self assert: (groupsOf2 allSatisfy: [ :array | array size = 2 ]).
	
	groupsOf2 := self nonEmptyMoreThan1Element groupsOf: 3 atATimeCollect: #yourself.
	
	self assert: groupsOf2 size equals: self nonEmptyMoreThan1Element size // 3.
	self assert: (groupsOf2 allSatisfy: [ :array | array size = 3 ]).
]

{ #category : #'tests - iterate on sequenced reable collections' }
TIterateSequencedReadableTest >> testKeysAndValuesDo [
	"| result |
	result:= OrderedCollection new.
	
	self nonEmptyMoreThan1Element  keysAndValuesDo: 
		[:i :value|
		result add: (value+i)].
	
	1 to: result size do:
		[:i|
		self assert: (result at:i)=((self nonEmptyMoreThan1Element at:i)+i)]"
	|  indexes elements |
	indexes := OrderedCollection new.
	elements := OrderedCollection new.
	
	self nonEmptyMoreThan1Element  keysAndValuesDo: 
		[:i :value|
		indexes  add: (i).
		elements add: value].
	
	(1 to: self nonEmptyMoreThan1Element size )do:
		[ :i |
		self assert: (indexes at: i) equals: i.
		self assert: (elements at: i) equals: (self nonEmptyMoreThan1Element at: i).	
		].
	
	self assert: indexes size equals: elements size. 
	self assert: indexes size equals: self nonEmptyMoreThan1Element size 
	
	
]

{ #category : #'tests - iterate on sequenced reable collections' }
TIterateSequencedReadableTest >> testKeysAndValuesDoEmpty [
	| result |
	result := OrderedCollection new.

	self empty keysAndValuesDo: [ :i :value | result add: value + i ].

	self assertEmpty: result
]

{ #category : #'tests - iterate on sequenced reable collections' }
TIterateSequencedReadableTest >> testPairsCollect [
	| index result |
	index := 0.

	result := self nonEmptyMoreThan1Element
		pairsCollect: [ :each1 :each2 | 
			self assert: (self nonEmptyMoreThan1Element indexOf: each2) equals: (index := index + 2).
			(self nonEmptyMoreThan1Element indexOf: each2) = ((self nonEmptyMoreThan1Element indexOf: each1) + 1) ].

	result do: [ :each | self assert: each ]
]

{ #category : #'tests - iterate on sequenced reable collections' }
TIterateSequencedReadableTest >> testPairsDo [

	| index |
	index:=1.
	
	self nonEmptyMoreThan1Element  pairsDo: 
		[:each1 :each2 | 
		self assert: (self nonEmptyMoreThan1Element at:index) equals: each1.
		self assert: (self nonEmptyMoreThan1Element at:(index+1)) equals: each2.
		index := index + 2 ].
	
	self nonEmptyMoreThan1Element size odd
		ifTrue:[ self assert: index equals: self nonEmptyMoreThan1Element size ]
		ifFalse:[ self assert: index equals: (self nonEmptyMoreThan1Element size+1) ]
]

{ #category : #'tests - iterate on sequenced reable collections' }
TIterateSequencedReadableTest >> testReverseDo [
	| result |
	result := OrderedCollection new.
	self nonEmpty reverseDo: [: each | result add: each].
	
	1 to: result size do:
		[:i|
		self assert: (result at: i)=(self nonEmpty at:(self nonEmpty size-i+1))]
]

{ #category : #'tests - iterate on sequenced reable collections' }
TIterateSequencedReadableTest >> testReverseDoEmpty [
	| result |
	result := OrderedCollection new.
	self empty reverseDo: [ :each | result add: each ].

	self assertEmpty: result
]

{ #category : #'tests - iterate on sequenced reable collections' }
TIterateSequencedReadableTest >> testReverseWithDo [
	
	| firstCollection secondCollection index |

	firstCollection := self nonEmptyMoreThan1Element.
	secondCollection := firstCollection copy.
	index := firstCollection size.
	
	firstCollection  reverseWith: secondCollection do:
		[:a :b |
	
		self assert: (firstCollection at: index) equals: a. 
		self assert: (secondCollection at: index) equals: b.
			(index := index - 1)]
	
	
	
]

{ #category : #'tests - iterate on sequenced reable collections' }
TIterateSequencedReadableTest >> testWithCollect [
	
	| result firstCollection secondCollection index  |

	firstCollection := self nonEmptyMoreThan1Element.
	secondCollection := firstCollection copy.
	index := 0.
	
	result := firstCollection  with: secondCollection collect:
		[:a :b |
		( index := index + 1).
		self assert: (firstCollection at: index) equals:  a. 
		self assert: (secondCollection at: index) equals:  b.
		b].
	
	result withIndexDo: [:el :i | self assert: (result at:i) equals: (secondCollection  at: i)].
	self assert: result size = secondCollection size
]

{ #category : #'tests - iterate on sequenced reable collections' }
TIterateSequencedReadableTest >> testWithCollectError [

	self should: [ self nonEmptyMoreThan1Element with: self empty collect:[:a :b | ]] raise: Error
]

{ #category : #'tests - iterate on sequenced reable collections' }
TIterateSequencedReadableTest >> testWithDo [
	
	| firstCollection secondCollection index |

	firstCollection := self nonEmptyMoreThan1Element.
	secondCollection := firstCollection copy.
	index := 0.
	
	firstCollection  with: secondCollection do:
		[:a :b |
		index := index + 1.
		self assert: (firstCollection at: index) equals:  a. 
		self assert: (secondCollection at: index) equals: b ]
	
	
]

{ #category : #'tests - iterate on sequenced reable collections' }
TIterateSequencedReadableTest >> testWithDoError [
	
	self should: [self nonEmptyMoreThan1Element with: self empty do:[:a :b | ]] raise: Error
]

{ #category : #'tests - iterate on sequenced reable collections' }
TIterateSequencedReadableTest >> testWithIndexCollect [
	
	| result index collection |
	index := 0.
	collection := self nonEmptyMoreThan1Element .
	result := collection  withIndexCollect: [:each :i | 
		self assert: i equals: (index := index + 1).	
		self assert: i equals: (collection  indexOf: each).
		each ]. 
	
	result withIndexDo: [:el :i | self assert: (result at:i) equals: (collection at: i)].
	self assert: result size equals: collection size 
]

{ #category : #'tests - iterate on sequenced reable collections' }
TIterateSequencedReadableTest >> testWithIndexDo [

	|  indexes elements |
	indexes:= OrderedCollection new.
	elements := OrderedCollection new.
	
	self nonEmptyMoreThan1Element withIndexDo: 
		[:value :i  |
			indexes  add: (i).
			elements add: value ].
	
	self nonEmptyMoreThan1Element withIndexDo: 
		[:el :i |
			self assert: (indexes at: i) equals: i.
			self assert: (elements at: i) equals: el ].
	
	self assert: indexes size equals: elements size. 
	self assert: indexes size equals: self nonEmptyMoreThan1Element size
	
]
