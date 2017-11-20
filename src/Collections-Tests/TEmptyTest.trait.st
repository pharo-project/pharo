"
I group a set of tests testing whether a collection is empty.
I do not do any assymption on the elements being added since it would
bind me to know how to access it. 

"
Trait {
	#name : #TEmptyTest,
	#category : #Collections-Tests-Abstract
}

{ #category : #requirements }
TEmptyTest >> empty [
	
	^ self explicitRequirement
]

{ #category : #requirements }
TEmptyTest >> nonEmpty [
	
	^ self explicitRequirement
]

{ #category : #'tests - fixture' }
TEmptyTest >> test0FixtureEmptyTest [
	self nonEmpty.
	self deny: self nonEmpty isEmpty.
	self empty.
	self assert: self empty isEmpty
]

{ #category : #'tests - empty' }
TEmptyTest >> testIfEmpty [

	self nonEmpty ifEmpty: [ self assert: false] .
	self empty ifEmpty: [ self assert: true] .
	

	
]

{ #category : #'tests - empty' }
TEmptyTest >> testIfEmptyifNotEmpty [

	self assert: (self empty ifEmpty: [true] ifNotEmpty: [false]).
	self assert: (self nonEmpty ifEmpty: [false] ifNotEmpty: [true]).
	
]

{ #category : #'tests - empty' }
TEmptyTest >> testIfNotEmpty [

	self empty ifNotEmpty: [self assert: false].
	self nonEmpty ifNotEmpty: [self assert: true].
	self assert: (self nonEmpty ifNotEmpty: [:s | s ]) = self nonEmpty
	
]

{ #category : #'tests - empty' }
TEmptyTest >> testIfNotEmptyifEmpty [

	self assert: (self empty ifNotEmpty: [false] ifEmpty: [true]).
	self assert: (self nonEmpty ifNotEmpty: [true] ifEmpty: [false]).
	
]

{ #category : #'tests - empty' }
TEmptyTest >> testIsEmpty [

	self assert: (self empty isEmpty).
	self deny: (self nonEmpty isEmpty).
]

{ #category : #'tests - empty' }
TEmptyTest >> testIsEmptyOrNil [

	self assert: (self empty isEmptyOrNil).
	self deny: (self nonEmpty isEmptyOrNil).
]

{ #category : #'tests - empty' }
TEmptyTest >> testNotEmpty [

	self assert: (self nonEmpty  notEmpty).
	self deny: (self empty notEmpty).
]
