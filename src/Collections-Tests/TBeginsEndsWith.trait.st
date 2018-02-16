"
A trait for test purposes
"
Trait {
	#name : #TBeginsEndsWith,
	#category : #'Collections-Tests-Abstract'
}

{ #category : #requirements }
TBeginsEndsWith >> empty [
	self explicitRequirement.
]

{ #category : #requirements }
TBeginsEndsWith >> nonEmpty [
	self explicitRequirement.
]

{ #category : #'tests - fixture' }
TBeginsEndsWith >> test0FixtureBeginsEndsWithTest [

	self nonEmpty.
	self deny: self nonEmpty isEmpty.
	self assert: self nonEmpty size > 1.
	self empty.
	self assert: self empty isEmpty
]

{ #category : #'tests - begins ends with' }
TBeginsEndsWith >> testsBeginsWith [
	
	self assert: (self nonEmpty beginsWith: (self nonEmpty copyUpTo: self nonEmpty last)).
	self assert: (self nonEmpty beginsWith: self nonEmpty).
	self deny: (self nonEmpty beginsWith: (self nonEmpty copyWith: self nonEmpty first))
]

{ #category : #'tests - begins ends with' }
TBeginsEndsWith >> testsBeginsWithEmpty [
	
	self deny: (self nonEmpty beginsWith:(self empty)).
	self deny: (self empty beginsWith:(self nonEmpty))

]

{ #category : #'tests - begins ends with' }
TBeginsEndsWith >> testsEndsWith [
	
	self assert: (self nonEmpty endsWith: self nonEmpty copyWithoutFirst).
	self assert: (self nonEmpty endsWith: self nonEmpty).
	self deny: (self nonEmpty endsWith: (self nonEmpty copyWith: self nonEmpty first))
]

{ #category : #'tests - begins ends with' }
TBeginsEndsWith >> testsEndsWithEmpty [
	
	self deny: (self nonEmpty endsWith: self empty).
	self deny: (self empty endsWith: self nonEmpty)
	
]
