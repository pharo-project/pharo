"
A trait for test purposes
"
Trait {
	#name : #TGrowableTest,
	#category : #'Collections-Tests-Abstract'
}

{ #category : #requirements }
TGrowableTest >> element [

	^ self explicitRequirement 
]

{ #category : #requirements }
TGrowableTest >> elementNotIn [
	
	^ self explicitRequirement
]

{ #category : #requirements }
TGrowableTest >> empty [ 

	^ self explicitRequirement
]

{ #category : #requirements }
TGrowableTest >> nonEmpty [ 

	^ self explicitRequirement
]

{ #category : #'tests - fixture' }
TGrowableTest >> test0FixtureRequirementsOfTGrowableTest [
	self empty.
	self nonEmpty.
	self element.
	self elementNotIn.
	self assert: self empty isEmpty.
	self deny: self nonEmpty isEmpty.
	self assert: (self nonEmpty includes: self element).
	self deny: (self nonEmpty includes: self elementNotIn)
]

{ #category : #'tests - growable' }
TGrowableTest >> testAddEmptyGrows [
	
	| oldSize |
	oldSize := self empty size.
	self empty add: self element.
	self assert: (self empty size) = (oldSize + 1).
]

{ #category : #'tests - growable' }
TGrowableTest >> testAddNonEmptyGrowsWhenNewElement [
	
	| oldSize |
	oldSize := self nonEmpty size.
	
	self nonEmpty add: self elementNotIn.
	self assert: (self nonEmpty size) > oldSize.
]
