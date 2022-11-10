"
Experimental

This is disabled by default.

If you want to experiment, you can enable the #optionCleanBlockClosure compiler option.

In the current system (with that option disabled), blocks are compiled to instances of CompiledBlock.
They are stored as literals and we create/push a FullBlockClosure with a special bytecode.

This bytecode thus creates a FullBock at runtime. 

If a block is clean (does not access self, super, outer temps/args, does not return), we can create the block
at compile time instead. 

CleanBlockClosure is exactly that: a closure created at compile time, it is stored in the literal frame and just
pushed on the stack.

For debugging, it does implement all needed machinary to read temps (via tempNamed:) just like a FullBlockClosure
"
Class {
	#name : #CleanBlockClosure,
	#superclass : #BlockClosure,
	#type : #variable,
	#instVars : [
		'ivarNeededByVMToBeChecked'
	],
	#category : #'Kernel-Methods'
}

{ #category : #accessing }
CleanBlockClosure >> IvarNeededByVMToBeChecked [
	"Clean blocks do not store a receiver, but it seems that the VM expects closures to have 4 ivars
	We should check if we can fix that.
	To see the problem: remove the var from CleanBlockClosure, enable the clean block option and recompile. Vm will crash"
	^ivarNeededByVMToBeChecked
]

{ #category : #printing }
CleanBlockClosure >> doPrintOn: aStream [

	aStream nextPutAll: self sourceNode value sourceCode
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
CleanBlockClosure >> literalsEvenTheOnesInTheInnerBlocks [
	^self compiledBlock literalsEvenTheOnesInTheInnerBlocks
]

{ #category : #accessing }
CleanBlockClosure >> messages [ 
	^self compiledBlock messages
]

{ #category : #accessing }
CleanBlockClosure >> outerCode [
	^self compiledBlock outerCode
]

{ #category : #accessing }
CleanBlockClosure >> outerCode: aCompiledCode [
	self compiledBlock outerCode: aCompiledCode
]

{ #category : #scanning }
CleanBlockClosure >> readsField: varIndex [ 
	^self compiledBlock readsField: varIndex
]

{ #category : #scanning }
CleanBlockClosure >> readsSelf [
	^self compiledBlock readsSelf
]

{ #category : #scanning }
CleanBlockClosure >> readsThisContext [
	^self compiledBlock readsThisContext
]

{ #category : #accessing }
CleanBlockClosure >> receiver [
	"Clean blocks do not know the receiver"
	^ nil
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
