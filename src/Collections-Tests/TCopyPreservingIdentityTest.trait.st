"
A trait for test purposes
"
Trait {
	#name : #TCopyPreservingIdentityTest,
	#category : #Collections-Tests-Abstract
}

{ #category : #requirements }
TCopyPreservingIdentityTest >> nonEmpty [
	self explicitRequirement
]

{ #category : #'tests - copy' }
TCopyPreservingIdentityTest >> testCopyReturnsIdentity [
	"self debug: #testCopyReturnsIdentity"
	
	| copy | 
	copy := self nonEmpty copy.
	self assert: self nonEmpty == copy.
	
]
