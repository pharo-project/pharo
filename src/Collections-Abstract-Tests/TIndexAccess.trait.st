"
A trait for test purposes
"
Trait {
	#name : 'TIndexAccess',
	#category : 'Collections-Abstract-Tests-Traits',
	#package : 'Collections-Abstract-Tests',
	#tag : 'Traits'
}

{ #category : 'requirements' }
TIndexAccess >> collectionMoreThan1NoDuplicates [
	" return a collection of size > 1 without equal elements"
	self explicitRequirement
]

{ #category : 'requirements' }
TIndexAccess >> elementInForIndexAccessing [
" return an element included in 'collectionMoreThan1NoDuplicates' "
	self explicitRequirement
]

{ #category : 'requirements' }
TIndexAccess >> elementNotInForIndexAccessing [
" return an element not included in 'collectionMoreThan1NoDuplicates' "
	self explicitRequirement
]

{ #category : 'tests - fixture' }
TIndexAccess >> test0FixtureIndexAccessTest [
	| res |
	self collectionMoreThan1NoDuplicates.
	self assert: self collectionMoreThan1NoDuplicates size > 1.
	res := true.
	self collectionMoreThan1NoDuplicates
		detect: [ :each | (self collectionMoreThan1NoDuplicates occurrencesOf: each) > 1 ]
		ifNone: [ res := false ].
	self assert: res = false.
	self elementInForIndexAccessing.
	self assert: (self collectionMoreThan1NoDuplicates includes: self elementInForIndexAccessing).
	self elementNotInForIndexAccessing.
	self deny: (self collectionMoreThan1NoDuplicates includes: self elementNotInForIndexAccessing)
]

{ #category : 'tests - index access' }
TIndexAccess >> testIdentityIndexOf [

	| collection element |
	collection := self collectionMoreThan1NoDuplicates.
	element := collection first.
	self assert: (collection identityIndexOf: element) equals: (collection indexOf: element)
]

{ #category : 'tests - index access' }
TIndexAccess >> testIdentityIndexOfIAbsent [
	| collection element |
	collection := self collectionMoreThan1NoDuplicates.
	element := collection first.
	self assert: (collection identityIndexOf: element ifAbsent: [ 0 ])
	     equals: 1.

	self assert: (collection identityIndexOf: self elementNotInForIndexAccessing ifAbsent: [ 55 ])
		  equals: 55
]

{ #category : 'tests - index access' }
TIndexAccess >> testIndexOf [

	| tmp index collection |
	collection := self collectionMoreThan1NoDuplicates.
	tmp := collection size.
	collection reverseDo:
		[ :each |
		each = self elementInForIndexAccessing ifTrue: [ index := tmp ].
		tmp := tmp - 1 ].
	self assert: (collection indexOf: self elementInForIndexAccessing) equals: index
]

{ #category : 'tests - index access' }
TIndexAccess >> testIndexOfIfAbsent [

	| collection |
	collection := self collectionMoreThan1NoDuplicates.
	self assert: (collection
			indexOf: collection first
			ifAbsent: [ 33 ]) equals: 1.
	self assert: (collection
			indexOf: self elementNotInForIndexAccessing
			ifAbsent: [ 33 ]) equals: 33
]

{ #category : 'tests - index access' }
TIndexAccess >> testIndexOfIfAbsentUsing [

	| collection equalityComparisonBlock identityComparisonBlock |
	collection := self collectionMoreThan1NoDuplicates.
	equalityComparisonBlock := [ : a : b | a = b ].
	identityComparisonBlock := [ : a : b | a == b ].

	self assert: (collection
			indexOf: collection first
			ifAbsent: [ 33 ]
			using: equalityComparisonBlock) equals: 1.
	self assert: (collection
			indexOf: self elementNotInForIndexAccessing
			ifAbsent: [ 33 ]
			using: equalityComparisonBlock) equals: 33.
			
	self assert: (collection
			indexOf: collection first
			ifAbsent: [ 33 ]
			using: identityComparisonBlock) equals: 1.
	self assert: (collection
			indexOf: self elementNotInForIndexAccessing
			ifAbsent: [ 33 ]
			using: identityComparisonBlock) equals: 33
]

{ #category : 'tests - index access' }
TIndexAccess >> testIndexOfStartingAt [

	| element collection |
	collection := self collectionMoreThan1NoDuplicates.
	element := collection first.
	self assert: (collection
			indexOf: element
			startingAt: 2
			ifAbsent: [ 99 ]) equals: 99.
	self assert: (collection
			indexOf: element
			startingAt: 1
			ifAbsent: [ 99 ]) equals: 1.
	self assert: (collection
			indexOf: self elementNotInForIndexAccessing
			startingAt: 1
			ifAbsent: [ 99 ]) equals: 99
]

{ #category : 'tests - index access' }
TIndexAccess >> testIndexOfStartingAtIfAbsent [

	| element collection |
	collection := self collectionMoreThan1NoDuplicates.
	element := collection first.
	self assert: (collection
			indexOf: element
			startingAt: 2
			ifAbsent: [ 99 ]) equals: 99.
	self assert: (collection
			indexOf: element
			startingAt: 1
			ifAbsent: [ 99 ]) equals: 1.
	self assert: (collection
			indexOf: self elementNotInForIndexAccessing
			startingAt: 1
			ifAbsent: [ 99 ]) equals: 99
]

{ #category : 'tests - index access' }
TIndexAccess >> testIndexOfStartingAtIfAbsentUsing [

	| element collection equalityComparisonBlock identityComparisonBlock |
	collection := self collectionMoreThan1NoDuplicates.
	element := collection first.
	equalityComparisonBlock := [ : a : b | a = b ].
	identityComparisonBlock := [ : a : b | a == b ].
	self assert: (collection
			indexOf: element
			startingAt: 2
			ifAbsent: [ 99 ]
			using: equalityComparisonBlock) equals: 99.
	self assert: (collection
			indexOf: element
			startingAt: 1
			ifAbsent: [ 99 ]
			using: equalityComparisonBlock) equals: 1.
	self assert: (collection
			indexOf: self elementNotInForIndexAccessing
			startingAt: 1
			ifAbsent: [ 99 ]
			using: equalityComparisonBlock) equals: 99.
			
	self assert: (collection
			indexOf: element
			startingAt: 2
			ifAbsent: [ 99 ]
			using: identityComparisonBlock) equals: 99.
	self assert: (collection
			indexOf: element
			startingAt: 1
			ifAbsent: [ 99 ]
			using: identityComparisonBlock) equals: 1.
	self assert: (collection
			indexOf: self elementNotInForIndexAccessing
			startingAt: 1
			ifAbsent: [ 99 ]
			using: identityComparisonBlock) equals: 99
]

{ #category : 'tests - index access' }
TIndexAccess >> testIndexOfSubCollectionStartingAt [

	| subcollection index collection |
	collection := self collectionMoreThan1NoDuplicates.
	subcollection := self collectionMoreThan1NoDuplicates.
	index := collection
		indexOfSubCollection: subcollection
		startingAt: 1.
	self assert: index equals: 1.
	index := collection
		indexOfSubCollection: subcollection
		startingAt: 2.
	self assert: index equals: 0
]

{ #category : 'tests - index access' }
TIndexAccess >> testIndexOfSubCollectionStartingAtIfAbsent [
	| absent subcollection collection |
	collection := self collectionMoreThan1NoDuplicates.
	subcollection := self collectionMoreThan1NoDuplicates.
	absent := false.
	collection indexOfSubCollection: subcollection startingAt: 1 ifAbsent: [ absent := true ].
	self deny: absent.
	absent := false.
	collection indexOfSubCollection: subcollection startingAt: 2 ifAbsent: [ absent := true ].
	self assert: absent
]

{ #category : 'tests - index access' }
TIndexAccess >> testLastIndexOf [

	| element collection |
	collection := self collectionMoreThan1NoDuplicates.
	element := collection first.
	self assert: (collection lastIndexOf: element) = 1.
	self assert: (collection lastIndexOf: self elementNotInForIndexAccessing) = 0
]

{ #category : 'tests - index access' }
TIndexAccess >> testLastIndexOfIfAbsent [

	| element collection |
	collection := self collectionMoreThan1NoDuplicates.
	element := collection first.
	self assert: (collection
			lastIndexOf: element
			ifAbsent: [ 99 ]) equals: 1.
	self assert: (collection
			lastIndexOf: self elementNotInForIndexAccessing
			ifAbsent: [ 99 ]) equals: 99
]

{ #category : 'tests - index access' }
TIndexAccess >> testLastIndexOfStartingAt [

	| element collection |
	collection := self collectionMoreThan1NoDuplicates.
	element := collection last.
	self assert: (collection
			lastIndexOf: element
			startingAt: collection size
			ifAbsent: [ 99 ]) equals: collection size.
	self assert: (collection
			lastIndexOf: element
			startingAt: collection size - 1
			ifAbsent: [ 99 ]) equals: 99.
	self assert: (collection
			lastIndexOf: self elementNotInForIndexAccessing
			startingAt: collection size
			ifAbsent: [ 99 ]) equals: 99
]
