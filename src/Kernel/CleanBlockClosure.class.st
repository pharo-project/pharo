"
!!!! Highly Experimental!!!

This is disabled by default.

If you want to experiment, you can enable the #optionCleanBlockClosure compiler option.

- This should not be a subclass of FullBlock (as it does not need the receiver ivar, for example)
- Debugging is not yet working as #pcInOuter needs to be implemented
"
Class {
	#name : #CleanBlockClosure,
	#superclass : #FullBlockClosure,
	#type : #variable,
	#category : #'Kernel-Methods'
}

{ #category : #accessing }
CleanBlockClosure >> hasLiteral: aLiteral [
	^self compiledBlock hasLiteral: aLiteral
]

{ #category : #accessing }
CleanBlockClosure >> hasLiteralSuchThat: aLiteral [
	^self compiledBlock hasLiteralSuchThat: aLiteral
]

{ #category : #accessing }
CleanBlockClosure >> innerCompiledBlocksAnySatisfy: aBlock [
	^self compiledBlock innerCompiledBlocksAnySatisfy: aBlock
]

{ #category : #accessing }
CleanBlockClosure >> innerCompiledBlocksDo: aBlock [
	^self compiledBlock innerCompiledBlocksDo: aBlock
]

{ #category : #testing }
CleanBlockClosure >> isEmbeddedBlock [
	^ true
]

{ #category : #accessing }
CleanBlockClosure >> messages [ 
	^self compiledBlock messages
]

{ #category : #accessing }
CleanBlockClosure >> outerCode: aCompiledCode [
	self compiledBlock outerCode: aCompiledCode
]

{ #category : #accessing }
CleanBlockClosure >> refersToLiteral: aLiteral [
	^self compiledBlock refersToLiteral: aLiteral
]

{ #category : #accessing }
CleanBlockClosure >> sendsAnySelectorOf: aCollection [
	^self compiledBlock sendsAnySelectorOf: aCollection
]
