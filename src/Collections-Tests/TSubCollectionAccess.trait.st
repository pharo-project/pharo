"
A trait for test purposes
"
Trait {
	#name : #TSubCollectionAccess,
	#category : #'Collections-Tests-Abstract'
}

{ #category : #requirements }
TSubCollectionAccess >> moreThan3Elements [
	" return a collection including atLeast 3 elements"
	self explicitRequirement
]

{ #category : #'tests - fixture' }
TSubCollectionAccess >> test0FixtureSubcollectionAccessTest [
	self moreThan3Elements.
	self assert: self moreThan3Elements size > 2
]

{ #category : #'tests - subcollections access' }
TSubCollectionAccess >> testAllButFirst [

	| abf col |
	col := self moreThan3Elements.
	abf := col allButFirst.
	self deny: abf first = col first.
	self assert: abf size + 1 = col size
]

{ #category : #'tests - subcollections access' }
TSubCollectionAccess >> testAllButFirstNElements [

	| abf col |
	col := self moreThan3Elements.
	abf := col allButFirst: 2.
	1 
		to: abf size
		do: [ :i | self assert: (abf at: i) = (col at: i + 2) ].
	self assert: abf size + 2 = col size
]

{ #category : #'tests - subcollections access' }
TSubCollectionAccess >> testAllButLast [

	| abf col |
	col := self moreThan3Elements.
	abf := col allButLast.
	self deny: abf last = col last.
	self assert: abf size + 1 = col size
]

{ #category : #'tests - subcollections access' }
TSubCollectionAccess >> testAllButLastNElements [

	| abf col |
	col := self moreThan3Elements.
	abf := col allButLast: 2.
	1 
		to: abf size
		do: [ :i | self assert: (abf at: i) = (col at: i) ].
	self assert: abf size + 2 = col size
]

{ #category : #'tests - subcollections access' }
TSubCollectionAccess >> testFirstNElements [

	| result |
	result := self moreThan3Elements first: self moreThan3Elements size - 1.
	1 to: result size do: [ :i | self assert: (result at: i) = (self moreThan3Elements at: i) ].
	self assert: result size = (self moreThan3Elements size - 1).
	self should: [ self moreThan3Elements first: self moreThan3Elements size + 1 ] raise: SubscriptOutOfBounds
]

{ #category : #'tests - subcollections access' }
TSubCollectionAccess >> testLastNElements [

	| result |
	result := self moreThan3Elements last: self moreThan3Elements size - 1.
	1 
		to: result size
		do: [ :i | self assert: (result at: i) = (self moreThan3Elements at: i + 1) ].
	self assert: result size = (self moreThan3Elements size - 1).
	self 
		should: [ self moreThan3Elements last: self moreThan3Elements size + 1 ]
		raise: Error
]
