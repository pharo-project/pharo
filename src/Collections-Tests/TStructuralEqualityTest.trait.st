"
Tests whether two objects are equals: they contain the same objects in the same order.
"
Trait {
	#name : #TStructuralEqualityTest,
	#category : #'Collections-Tests-Abstract'
}

{ #category : #'test - equality' }
TStructuralEqualityTest >> empty [
	
	^ self explicitRequirement
]

{ #category : #'test - equality' }
TStructuralEqualityTest >> nonEmpty [
	
	^ self explicitRequirement
]

{ #category : #'tests - fixture' }
TStructuralEqualityTest >> test0TStructuralEqualityTest [
	self empty.
	self nonEmpty.
	self assert: self empty isEmpty.
	self deny: self nonEmpty isEmpty
]

{ #category : #'test - equality' }
TStructuralEqualityTest >> testEqualSign [
 
	self deny: (self empty = self nonEmpty)
]

{ #category : #'test - equality' }
TStructuralEqualityTest >> testEqualSignIsTrueForNonIdenticalButEqualCollections [
		
	self assert: self empty equals: self empty copy. 
	self assert: self empty copy equals: self empty.
	self assert: self empty copy equals: self empty copy.
		
	self assert: self nonEmpty equals: self nonEmpty copy. 
	self assert: self nonEmpty copy equals: self nonEmpty.
	self assert: self nonEmpty copy equals: self nonEmpty copy
]

{ #category : #'test - equality' }
TStructuralEqualityTest >> testEqualSignOfIdenticalCollectionObjects [
	
	self assert: self empty equals: self empty. 
	self assert: self nonEmpty equals: self nonEmpty 
	
]
