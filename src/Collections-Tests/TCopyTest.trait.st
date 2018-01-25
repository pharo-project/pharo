"
Tests whether a copied object contains the same elements than its source.
Note that the order is not garanteed -- see TEqualityTest for that. 


"
Trait {
	#name : #TCopyTest,
	#category : #'Collections-Tests-Abstract'
}

{ #category : #requirements }
TCopyTest >> collectionNotIncluded [
" return a collection for wich each element is not included in 'nonEmpty' "
	^ self explicitRequirement
]

{ #category : #requirements }
TCopyTest >> collectionWithElementsToRemove [
" return a collection of elements included in 'nonEmpty'  "
	^ self explicitRequirement
]

{ #category : #requirements }
TCopyTest >> elementToAdd [
" return an element of type 'nonEmpy' elements'type'  not  yet included in nonEmpty" 
	^ self explicitRequirement
]

{ #category : #requirements }
TCopyTest >> empty [
	^ self explicitRequirement
]

{ #category : #requirements }
TCopyTest >> nonEmpty [
	^ self explicitRequirement
]

{ #category : #'tests - fixture' }
TCopyTest >> test0CopyTest [
	self empty.
	self assert: self empty size = 0.
	self nonEmpty.
	self assert: (self nonEmpty size = 0) not.
	self collectionWithElementsToRemove.
	self assert: (self collectionWithElementsToRemove size = 0) not.
	self collectionWithElementsToRemove do: [ :each | self assert: (self nonEmpty includes: each) ].
	self elementToAdd.
	self deny: (self nonEmpty includes: self elementToAdd).
	self collectionNotIncluded.
	self collectionNotIncluded do: [ :each | self deny: (self nonEmpty includes: each) ]
]

{ #category : #'tests - copy' }
TCopyTest >> testCopyEmptyWith [
	| res anElement |
	anElement := self elementToAdd.
	res := self empty copyWith: anElement.
	self assert: res size = (self empty size + 1).
	self assert: (res includes: (anElement value))
]

{ #category : #'tests - copy' }
TCopyTest >> testCopyEmptyWithout [
	| res |
	res := self empty copyWithout: self elementToAdd.
	self assert: res size = self empty size.
	self deny: (res includes: self elementToAdd)
]

{ #category : #'tests - copy' }
TCopyTest >> testCopyEmptyWithoutAll [
	| res |
	res := self empty copyWithoutAll: self collectionWithElementsToRemove.
	self assert: res size = self empty size.
	self collectionWithElementsToRemove do: [ :each | self deny: (res includes: each) ]
]

{ #category : #'tests - copy' }
TCopyTest >> testCopyEquals [
	"A copy should be equivalent to the things it's a copy of"
	
	| copy | 
	copy := self nonEmpty copy.
	self assert: copy  = self nonEmpty.
]

{ #category : #'tests - copy' }
TCopyTest >> testCopyNonEmptyWith [

	| res anElement |
	anElement := self elementToAdd .
	res := self nonEmpty copyWith: anElement.
	"here we do not test the size since for a non empty set we would get a problem.
	Then in addition copy is not about duplicate management. The element should 
	be in at the end."
	self assert: (res includes: (anElement value)).
	self nonEmpty do: [ :each | res includes: each ]
]

{ #category : #'tests - copy' }
TCopyTest >> testCopyNonEmptyWithout [
	
	| res anElementOfTheCollection |
	anElementOfTheCollection :=  self nonEmpty anyOne.
	res := (self nonEmpty copyWithout: anElementOfTheCollection).
	"here we do not test the size since for a non empty set we would get a problem.
	Then in addition copy is not about duplicate management. The element should 
	be in at the end."
	self deny: (res includes: anElementOfTheCollection).
	self nonEmpty do:
		[:each | (each = anElementOfTheCollection) 
					ifFalse: [self assert: (res includes: each)]].
	

]

{ #category : #'tests - copy' }
TCopyTest >> testCopyNonEmptyWithoutAll [
	| res |
	res := self nonEmpty copyWithoutAll: self collectionWithElementsToRemove.
	"here we do not test the size since for a non empty set we would get a problem.
	Then in addition copy is not about duplicate management. The element should 
	be in at the end."
	self collectionWithElementsToRemove do: [ :each | self deny: (res includes: (each)) ].
	self nonEmpty do: 
		[ :each | 
		(self collectionWithElementsToRemove includes: each) ifFalse: [ self assert: (res includes: each) ] ]
]

{ #category : #'tests - copy' }
TCopyTest >> testCopyNonEmptyWithoutAllNotIncluded [

	| res |
	res := self nonEmpty copyWithoutAll: self collectionNotIncluded.
	"here we do not test the size since for a non empty set we would get a problem.
	Then in addition copy is not about duplicate management. The element should 
	be in at the end."
	self nonEmpty do: [ :each | self assert: (res includes: each) ]
]

{ #category : #'tests - copy' }
TCopyTest >> testCopyNonEmptyWithoutNotIncluded [

	| res |
	res := self nonEmpty copyWithout: self elementToAdd.
	"here we do not test the size since for a non empty set we would get a problem.
	Then in addition copy is not about duplicate management. The element should 
	be in at the end."
	self nonEmpty do: [ :each | self assert: (res includes: each) ]
]

{ #category : #'tests - copy' }
TCopyTest >> testCopyNotSame [
	"A copy of a collection should always be of the same class as the instance it copies"
	
	| copy | 
	copy := self nonEmpty copy.
	self deny: copy  == self nonEmpty.
]

{ #category : #'tests - copy' }
TCopyTest >> testCopySameClass [
	"A copy of a collection should always be of the same class as the instance it copies"
	
	| copy | 
	copy := self empty copy.
	self assert: copy class == self empty class.
]
