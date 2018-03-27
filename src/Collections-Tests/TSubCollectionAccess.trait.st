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

	| allButFirst collection |
	collection := self moreThan3Elements.
	allButFirst := collection allButFirst: 2.
	allButFirst withIndexDo: 
		 [:el :i | self assert: el equals: (collection at: i + 2) ].
	self assert: allButFirst size + 2 equals: collection size
]

{ #category : #'tests - subcollections access' }
TSubCollectionAccess >> testAllButLast [

	| abf col |
	col := self moreThan3Elements.
	abf := col allButLast.
	self deny: abf last = col last.
	self assert: abf size + 1 equals: col size
]

{ #category : #'tests - subcollections access' }
TSubCollectionAccess >> testAllButLastNElements [

	| allButLast collection |
	collection := self moreThan3Elements.
	allButLast := collection allButLast: 2.
	allButLast withIndexDo: [:el :i | self assert: el equals: (collection at: i) ].
	self assert: allButLast size + 2 equals: collection size
]

{ #category : #'tests - subcollections access' }
TSubCollectionAccess >> testFirstNElements [

	| result |
	result := self moreThan3Elements first: self moreThan3Elements size - 1.
	result withIndexDo: [:el :i | self assert: el equals: (self moreThan3Elements at: i) ].
	
	self assert: result size equals: (self moreThan3Elements size - 1).
	self should: [ self moreThan3Elements first: self moreThan3Elements size + 1 ] raise: SubscriptOutOfBounds
]

{ #category : #'tests - subcollections access' }
TSubCollectionAccess >> testLastNElements [

	| result |
	result := self moreThan3Elements last: self moreThan3Elements size - 1.
	result withIndexDo: [:el :i | self assert: el equals: (self moreThan3Elements at: i + 1) ].
	self assert: result size equals: (self moreThan3Elements size - 1).
	self 
		should: [ self moreThan3Elements last: self moreThan3Elements size + 1 ]
		raise: Error
]
