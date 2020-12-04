"
A trait for test purposes
"
Trait {
	#name : #TSizeTest,
	#category : #'Collections-Abstract-Tests-Traits'
}

{ #category : #requirements }
TSizeTest >> empty [

	^ self explicitRequirement
]

{ #category : #requirements }
TSizeTest >> sizeCollection [
	"Answers a collection not empty"
	^ self explicitRequirement
]

{ #category : #'tests - fixture' }
TSizeTest >> test0TSizeTest [
	self empty.
	self sizeCollection.
	self assertEmpty: self empty.
	self denyEmpty: self sizeCollection
]

{ #category : #'tests - size capacity' }
TSizeTest >> testSize [

	| size |
	self assert: self empty size = 0.
	size := 0.
	self sizeCollection do: [ :each | size := size + 1].
	
	self assert: self sizeCollection size = size.
]
