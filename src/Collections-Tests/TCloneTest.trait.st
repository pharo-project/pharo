"
A trait for test purposes
"
Trait {
	#name : #TCloneTest,
	#category : #'Collections-Tests-Abstract'
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
	self deny: self nonEmpty isEmpty.
	self empty.
	self assert: self empty isEmpty
]

{ #category : #'tests - copy - clone' }
TCloneTest >> testCopyCreatesNewObject [
	 
	
	| copy | 
	copy := self nonEmpty copy.
	self deny: self nonEmpty == copy.
	
]

{ #category : #'tests - copy - clone' }
TCloneTest >> testCopyEmpty [
	
	| copy | 
	copy := self empty copy.
	self assert: copy isEmpty.
]

{ #category : #'tests - copy - clone' }
TCloneTest >> testCopyNonEmpty [
	
	| copy | 
	copy := self nonEmpty copy.
	self deny: copy isEmpty.
	self assert: copy size = self nonEmpty size.
	self nonEmpty do: 
		[:each | copy includes: each]
]
