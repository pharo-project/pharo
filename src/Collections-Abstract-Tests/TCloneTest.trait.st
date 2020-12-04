"
A trait for test purposes
"
Trait {
	#name : #TCloneTest,
	#category : #'Collections-Abstract-Tests-Traits'
}

{ #category : #helper }
TCloneTest >> empty [

	^ self explicitRequirement
]

{ #category : #helper }
TCloneTest >> nonEmpty [

	^ self explicitRequirement
]

{ #category : #'tests - fixture' }
TCloneTest >> test0FixtureCloneTest [
	self nonEmpty.
	self denyEmpty: self nonEmpty.
	self empty.
	self assertEmpty: self empty
]

{ #category : #'tests - copy - clone' }
TCloneTest >> testCopyCreatesNewObject [
	 
	
	| copy | 
	copy := self nonEmpty copy.
	self deny: self nonEmpty == copy.
	
]

{ #category : #'tests - copy - clone' }
TCloneTest >> testCopyEmpty [
	self assertEmpty: self empty copy
]

{ #category : #'tests - copy - clone' }
TCloneTest >> testCopyNonEmpty [
	| copy |
	copy := self nonEmpty copy.
	self denyEmpty: copy.
	self assert: copy size = self nonEmpty size.
	self nonEmpty do: [ :each | copy includes: each ]
]
