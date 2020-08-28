"
Experimental

This is disabled by default.

If you want to experiment, you can enable the #optionCleanBlockClosure compiler option.

- This should not be a subclass of FullBlock (as it does not need the receiver ivar, for example)
"
Class {
	#name : #CleanBlockClosure,
	#superclass : #FullBlockClosure,
	#type : #variable,
	#category : #'Kernel-Methods'
}

{ #category : #adding }
CleanBlockClosure >> addWithAllBlocksInto: collected [
	^self compiledBlock addWithAllBlocksInto: collected
]

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
CleanBlockClosure >> isClean [
	^true
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

{ #category : #scanning }
CleanBlockClosure >> readsField: varIndex [ 
	^self compiledBlock readsField: varIndex
]

{ #category : #accessing }
CleanBlockClosure >> refersToLiteral: aLiteral [
	^self compiledBlock refersToLiteral: aLiteral
]

{ #category : #'debugger access' }
CleanBlockClosure >> sender [
	" clean blocks do not know the sender"
	^nil
]

{ #category : #accessing }
CleanBlockClosure >> sendsAnySelectorOf: aCollection [
	^self compiledBlock sendsAnySelectorOf: aCollection
]

{ #category : #scanning }
CleanBlockClosure >> sendsToSuper [
	^self compiledBlock sendsToSuper
]

{ #category : #scanning }
CleanBlockClosure >> writesField: varIndex [ 
	^self compiledBlock writesField: varIndex
]
