"
A trait for test purposes
"
Trait {
	#name : #TPutBasicTest,
	#category : #'Collections-Tests-Abstract'
}

{ #category : #requirements }
TPutBasicTest >> aValue [
" return a value to put into nonEmpty"
	^ self explicitRequirement
]

{ #category : #requirements }
TPutBasicTest >> anIndex [
" return an index in nonEmpty bounds"
	^ self explicitRequirement
]

{ #category : #requirements }
TPutBasicTest >> anotherValue [
" return a value ( not eual to 'aValue' ) to put into nonEmpty "
	^ self explicitRequirement
]

{ #category : #requirements }
TPutBasicTest >> empty [
	^self explicitRequirement.
]

{ #category : #requirements }
TPutBasicTest >> nonEmpty [

	^ self explicitRequirement
]

{ #category : #'tests - fixture' }
TPutBasicTest >> test0FixturePutTest [
	self aValue.
	self anotherValue.
	
	self anIndex.
	self nonEmpty isDictionary 
		ifFalse:[self assert: (self anIndex >=1 & self anIndex <= self nonEmpty size).].
	
	self empty.
	self assert: self empty isEmpty .
	
	self nonEmpty.
	self deny: self nonEmpty  isEmpty.
]

{ #category : #'tests - at put' }
TPutBasicTest >> testAtPut [
	
	self nonEmpty at: self anIndex put: self aValue.
	self assert: (self nonEmpty at: self anIndex) = self aValue.
	
]

{ #category : #'tests - at put' }
TPutBasicTest >> testAtPutOutOfBounds [
	
	self should: [self empty at: self anIndex put: self aValue] raise: Error
	
]

{ #category : #'tests - at put' }
TPutBasicTest >> testAtPutTwoValues [
	
	self nonEmpty at: self anIndex put: self aValue.
	self nonEmpty at: self anIndex put: self anotherValue.
	self assert: (self nonEmpty at: self anIndex) = self anotherValue.
]
