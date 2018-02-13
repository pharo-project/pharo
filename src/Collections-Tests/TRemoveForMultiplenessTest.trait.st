"
A trait for test purposes
"
Trait {
	#name : #TRemoveForMultiplenessTest,
	#traits : 'TRemoveTest',
	#classTraits : 'TRemoveTest classTrait',
	#category : #'Collections-Tests-Abstract'
}

{ #category : #requirements }
TRemoveForMultiplenessTest >> elementNotIn [
" return an element not included in nonEmptyWithoutEqualElements"
	self explicitRequirement
]

{ #category : #requirements }
TRemoveForMultiplenessTest >> elementTwiceIn [
	^ self explicitRequirement
]

{ #category : #requirements }
TRemoveForMultiplenessTest >> empty [
	self explicitRequirement
]

{ #category : #requirements }
TRemoveForMultiplenessTest >> nonEmptyWithoutEqualElements [
" return a collection without equal elements "
	self explicitRequirement
]

{ #category : #'tests - fixture' }
TRemoveForMultiplenessTest >> test0FixtureTRemoveTest [
	| duplicate |
	self empty.
	self nonEmptyWithoutEqualElements.
	self deny: self nonEmptyWithoutEqualElements isEmpty.
	duplicate := true.
	self nonEmptyWithoutEqualElements
		detect: [ :each | (self nonEmptyWithoutEqualElements occurrencesOf: each) > 1 ]
		ifNone: [ duplicate := false ].
	self assert: duplicate = false.
	self elementNotIn.
	self assert: self empty isEmpty.
	self deny: self nonEmptyWithoutEqualElements isEmpty.
	self deny: (self nonEmptyWithoutEqualElements includes: self elementNotIn)
]

{ #category : #'tests - remove' }
TRemoveForMultiplenessTest >> testRemoveAll [

	| el aSubCollection collection res |
	collection := self nonEmptyWithoutEqualElements.
	el := collection anyOne.
	aSubCollection := collection copyWithout: el.
	res := collection removeAll: aSubCollection.
	self assert: collection size = 1.
	self nonEmptyWithoutEqualElements do: [ :each | self assert: each = el ]
]

{ #category : #'tests - remove' }
TRemoveForMultiplenessTest >> testRemoveAllError [

	| el aSubCollection |
	el := self elementNotIn.
	aSubCollection := self nonEmptyWithoutEqualElements copyWith: el.
	self 
		should: [ | res | res := self nonEmptyWithoutEqualElements removeAll: aSubCollection ]
		raise: Error
]

{ #category : #'tests - remove' }
TRemoveForMultiplenessTest >> testRemoveAllFoundIn [

	| el aSubCollection res |
	el := self nonEmptyWithoutEqualElements anyOne.
	aSubCollection := (self nonEmptyWithoutEqualElements copyWithout: el) copyWith: self elementNotIn.
	res := self nonEmptyWithoutEqualElements removeAllFoundIn: aSubCollection.
	self assert: self nonEmptyWithoutEqualElements size = 1.
	self nonEmptyWithoutEqualElements do: [ :each | self assert: each = el ]
]

{ #category : #'tests - remove' }
TRemoveForMultiplenessTest >> testRemoveAllSuchThat [

	| el aSubCollection |
	el := self nonEmptyWithoutEqualElements anyOne.
	aSubCollection := self nonEmptyWithoutEqualElements copyWithout: el.
	self nonEmptyWithoutEqualElements removeAllSuchThat: [ :each | aSubCollection includes: each ].
	self assert: self nonEmptyWithoutEqualElements size = 1.
	self nonEmptyWithoutEqualElements do: [ :each | self assert: each = el ]
]

{ #category : #'tests - remove' }
TRemoveForMultiplenessTest >> testRemoveElementFromEmpty [
	 
	self 
		should: [ self empty remove: self nonEmptyWithoutEqualElements anyOne ]
		raise: Error
]

{ #category : #'tests - remove' }
TRemoveForMultiplenessTest >> testRemoveElementReallyRemovesElement [
 
	| size |
	size := self nonEmptyWithoutEqualElements size.
	self nonEmptyWithoutEqualElements remove: self nonEmptyWithoutEqualElements anyOne.
	self assert: size - 1 = self nonEmptyWithoutEqualElements size
]

{ #category : #'tests - remove' }
TRemoveForMultiplenessTest >> testRemoveElementThatExists [
 
	| el res |
	el := self nonEmptyWithoutEqualElements anyOne.
	res := self nonEmptyWithoutEqualElements remove: el.
	self assert: res == el
]

{ #category : #'tests - remove' }
TRemoveForMultiplenessTest >> testRemoveElementThatExistsTwice [

	| size |
	size := self nonEmpty size.
	self assert: (self nonEmpty includes: self elementTwiceIn).
	self nonEmpty remove: self elementTwiceIn.
	self assert: size - 1 = self nonEmpty size.
	
	self assert: (self nonEmpty includes: self elementTwiceIn).
	self nonEmpty remove: self elementTwiceIn.
	self assert: size - 2 = self nonEmpty size
]

{ #category : #'tests - remove' }
TRemoveForMultiplenessTest >> testRemoveIfAbsent [

	| el res |
	el := self elementNotIn.
	res := self nonEmptyWithoutEqualElements remove: el ifAbsent: [ 33 ].
	self assert: res = 33
]
