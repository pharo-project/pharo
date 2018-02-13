"
A trait for test purposes
"
Trait {
	#name : #TSequencedElementAccessTest,
	#category : #'Collections-Tests-Abstract'
}

{ #category : #requirements }
TSequencedElementAccessTest >> elementInForElementAccessing [
" return an element inculded in 'moreThan4Elements'"
	self explicitRequirement
]

{ #category : #requirements }
TSequencedElementAccessTest >> elementNotInForElementAccessing [
" return an element not included in 'moreThan4Elements' "
	self explicitRequirement
]

{ #category : #requirements }
TSequencedElementAccessTest >> moreThan4Elements [

" return a collection including at leat 4 elements"
	self explicitRequirement
]

{ #category : #requirements }
TSequencedElementAccessTest >> subCollectionNotIn [
" return a collection for which at least one element is not included in 'moreThan4Elements' "
	self explicitRequirement
]

{ #category : #'tests - fixture' }
TSequencedElementAccessTest >> test0FixtureSequencedElementAccessTest [
	self moreThan4Elements.
	self assert: self moreThan4Elements size >= 4.
	self subCollectionNotIn.
	self subCollectionNotIn detect: [ :each | (self moreThan4Elements includes: each) not ] ifNone: [ self assert: false ].
	self elementNotInForElementAccessing.
	self deny: (self moreThan4Elements includes: self elementNotInForElementAccessing).
	self elementInForElementAccessing.
	self assert: (self moreThan4Elements includes: self elementInForElementAccessing)
]

{ #category : #'tests - element accessing' }
TSequencedElementAccessTest >> testAfter [

	self assert: (self moreThan4Elements after: (self moreThan4Elements at: 1)) = (self moreThan4Elements at: 2).
	self 
		should: 
			[ self moreThan4Elements after: (self moreThan4Elements at: self moreThan4Elements size) ]
		raise: Error.
	self 
		should: [ self moreThan4Elements after: self elementNotInForElementAccessing ]
		raise: Error
]

{ #category : #'tests - element accessing' }
TSequencedElementAccessTest >> testAfterIfAbsent [

	self assert: (self moreThan4Elements 
			after: (self moreThan4Elements at: 1)
			ifAbsent: [ 33 ]) = (self moreThan4Elements at: 2).
	self assert: (self moreThan4Elements 
			after: (self moreThan4Elements at: self moreThan4Elements size)
			ifAbsent: [ 33 ]) = 33.
	self assert: (self moreThan4Elements 
			after: self elementNotInForElementAccessing
			ifAbsent: [ 33 ]) = 33
]

{ #category : #'tests - element accessing' }
TSequencedElementAccessTest >> testAt [
	"
	self assert: (self accessCollection at: 1) = 1.
	self assert: (self accessCollection at: 2) = 2.
	"
	| index |
	index := self moreThan4Elements indexOf: self elementInForElementAccessing.
	self assert: (self moreThan4Elements at: index) = self elementInForElementAccessing
]

{ #category : #'tests - element accessing' }
TSequencedElementAccessTest >> testAtAll [
	"	self flag: #theCollectionshouldbe102030intheFixture.
	
	self assert: (self accessCollection atAll: #(2 1)) first = self accessCollection second.
	self assert: (self accessCollection atAll: #(2)) first = self accessCollection second."
	| result |
	result := self moreThan4Elements atAll: #(2 1 2 ).
	self assert: (result at: 1) = (self moreThan4Elements at: 2).
	self assert: (result at: 2) = (self moreThan4Elements at: 1).
	self assert: (result at: 3) = (self moreThan4Elements at: 2).
	self assert: (self moreThan4Elements atAll: #()) = self moreThan4Elements species new
]

{ #category : #'tests - element accessing' }
TSequencedElementAccessTest >> testAtIfAbsent [
	| absent |
	absent := false.
	self moreThan4Elements 
		at: self moreThan4Elements size + 1
		ifAbsent: [ absent := true ].
	self assert: absent = true.
	absent := false.
	self moreThan4Elements 
		at: self moreThan4Elements size
		ifAbsent: [ absent := true ].
	self assert: absent = false
]

{ #category : #'tests - element accessing' }
TSequencedElementAccessTest >> testAtLast [

	| index |
	self assert: (self moreThan4Elements atLast: 1) = self moreThan4Elements last.
	"tmp:=1.
	self do:
		[:each | 
		each =self elementInForIndexAccessing 
			ifTrue:[index:=tmp].
		tmp:=tmp+1]."
	index := self moreThan4Elements indexOf: self elementInForElementAccessing.
	self assert: (self moreThan4Elements atLast: index) = (self moreThan4Elements at: self moreThan4Elements size - index + 1)
]

{ #category : #'tests - element accessing' }
TSequencedElementAccessTest >> testAtLastError [

	self 
		should: [ self moreThan4Elements atLast: self moreThan4Elements size + 1 ]
		raise: Error
]

{ #category : #'tests - element accessing' }
TSequencedElementAccessTest >> testAtLastIfAbsent [

	self assert: (self moreThan4Elements 
			atLast: 1
			ifAbsent: [ nil ]) = self moreThan4Elements last.
	self assert: (self moreThan4Elements 
			atLast: self moreThan4Elements size + 1
			ifAbsent: [ 222 ]) = 222
]

{ #category : #'tests - element accessing' }
TSequencedElementAccessTest >> testAtOutOfBounds [

	self 
		should: [ self moreThan4Elements at: self moreThan4Elements size + 1 ]
		raise: Error.
	self 
		should: [ self moreThan4Elements at: -1 ]
		raise: Error
]

{ #category : #'tests - element accessing' }
TSequencedElementAccessTest >> testAtPin [

	self assert: (self moreThan4Elements atPin: 2) = self moreThan4Elements second.
	self assert: (self moreThan4Elements atPin: 99) = self moreThan4Elements last.
	self assert: (self moreThan4Elements atPin: -99) = self moreThan4Elements first
]

{ #category : #'tests - element accessing' }
TSequencedElementAccessTest >> testAtRandom [
	| result |
	result := self nonEmpty atRandom .
	self assert: (self nonEmpty includes: result).
]

{ #category : #'tests - element accessing' }
TSequencedElementAccessTest >> testAtWrap [
	"
	self assert: (self accessCollection at: 1) = 1.
	self assert: (self accessCollection at: 2) = 2.
	"
	| index |
	index := self moreThan4Elements indexOf: self elementInForElementAccessing.
	self assert: (self moreThan4Elements atWrap: index) = self elementInForElementAccessing.
	self assert: (self moreThan4Elements atWrap: index + self moreThan4Elements size) = self elementInForElementAccessing.
	self assert: (self moreThan4Elements atWrap: index - self moreThan4Elements size) = self elementInForElementAccessing.
	self assert: (self moreThan4Elements atWrap: 1 + self moreThan4Elements size) = (self moreThan4Elements at: 1)
]

{ #category : #'tests - element accessing' }
TSequencedElementAccessTest >> testBefore [

	self assert: (self moreThan4Elements before: (self moreThan4Elements at: 2)) = (self moreThan4Elements at: 1).
	self 
		should: [ self moreThan4Elements before: (self moreThan4Elements at: 1) ]
		raise: Error.
	self 
		should: [ self moreThan4Elements before: 66 ]
		raise: Error
]

{ #category : #'tests - element accessing' }
TSequencedElementAccessTest >> testBeforeIfAbsent [

	self assert: (self moreThan4Elements 
			before: (self moreThan4Elements at: 1)
			ifAbsent: [ 99 ]) = 99.
	self assert: (self moreThan4Elements 
			before: (self moreThan4Elements at: 2)
			ifAbsent: [ 99 ]) = (self moreThan4Elements at: 1)
]

{ #category : #'tests - element accessing' }
TSequencedElementAccessTest >> testFirstSecondThird [

	self assert: self moreThan4Elements first = (self moreThan4Elements at: 1).
	self assert: self moreThan4Elements second = (self moreThan4Elements at: 2).
	self assert: self moreThan4Elements third = (self moreThan4Elements at: 3).
	self assert: self moreThan4Elements fourth = (self moreThan4Elements at: 4)
]

{ #category : #'tests - element accessing' }
TSequencedElementAccessTest >> testLast [

	self assert: self moreThan4Elements last = (self moreThan4Elements at: self moreThan4Elements size)
]

{ #category : #'tests - element accessing' }
TSequencedElementAccessTest >> testMiddle [

	self assert: self moreThan4Elements middle = (self moreThan4Elements at: self moreThan4Elements size // 2 + 1)
]
