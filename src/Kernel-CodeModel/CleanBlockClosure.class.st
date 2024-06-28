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
	#name : 'CleanBlockClosure',
	#superclass : 'BlockClosure',
	#type : 'variable',
	#instVars : [
		'literal'
	],
	#category : 'Kernel-CodeModel-Methods',
	#package : 'Kernel-CodeModel',
	#tag : 'Methods'
}

{ #category : 'accessing' }
CleanBlockClosure class >> compiledBlock: aCompiledBlock [
	^self new
		numArgs: aCompiledBlock numArgs;
		compiledBlock: aCompiledBlock
]

{ #category : 'printing' }
CleanBlockClosure >> doPrintOn: aStream [

	aStream nextPutAll: self sourceNode value sourceCode
]

{ #category : 'accessing' }
CleanBlockClosure >> hasLiteral: aLiteral [
	^self compiledBlock hasLiteral: aLiteral
]

{ #category : 'accessing' }
CleanBlockClosure >> hasLiteralSuchThat: aLiteral [
	^self compiledBlock hasLiteralSuchThat: aLiteral
]

{ #category : 'accessing' }
CleanBlockClosure >> innerCompiledBlocksAnySatisfy: aBlock [
	^self compiledBlock innerCompiledBlocksAnySatisfy: aBlock
]

{ #category : 'accessing' }
CleanBlockClosure >> innerCompiledBlocksDo: aBlock [
	^self compiledBlock innerCompiledBlocksDo: aBlock
]

{ #category : 'testing' }
CleanBlockClosure >> isClean [
	^true
]

{ #category : 'testing' }
CleanBlockClosure >> isEmbeddedBlock [
	^ true
]

{ #category : 'accessing' }
CleanBlockClosure >> literal [
	"The variable and accessor should be in the subclass ConstantBlockClosure.
	It seems that the VM expects closures to have 4 ivars
	We should check if we can fix that."
	^literal
]

{ #category : 'accessing' }
CleanBlockClosure >> literalsEvenTheOnesInTheInnerBlocks [
	^self compiledBlock literalsEvenTheOnesInTheInnerBlocks
]

{ #category : 'accessing' }
CleanBlockClosure >> messages [
	^self compiledBlock messages
]

{ #category : 'accessing' }
CleanBlockClosure >> outerCode [
	^self compiledBlock outerCode
]

{ #category : 'accessing' }
CleanBlockClosure >> outerCode: aCompiledCode [
	self compiledBlock outerCode: aCompiledCode
]

{ #category : 'accessing' }
CleanBlockClosure >> pathOfLiteralIndexes [
	
	^ self compiledBlock pathOfLiteralIndexes allButLast
]

{ #category : 'scanning' }
CleanBlockClosure >> readsField: varIndex [
	"Clean blocks by definition do not read fields"
	^false
]

{ #category : 'scanning' }
CleanBlockClosure >> readsSelf [
	"Clean blocks by definition do not access self"
	^false
]

{ #category : 'scanning' }
CleanBlockClosure >> readsThisContext [
	"Clean blocks can access thisContext, we have to forward to the compiled block to check"
	^self compiledBlock readsThisContext
]

{ #category : 'accessing' }
CleanBlockClosure >> receiver [
	"Clean blocks do not know the receiver"
	^ nil
]

{ #category : 'accessing' }
CleanBlockClosure >> refersToLiteral: aLiteral [
	^self compiledBlock refersToLiteral: aLiteral
]

{ #category : 'debugger access' }
CleanBlockClosure >> sender [
	" clean blocks do not know the sender"
	<reflection: 'Message sending and code execution - Message send reification'>
	^nil
]

{ #category : 'accessing' }
CleanBlockClosure >> sendsAnySelectorOf: aCollection [
	^self compiledBlock sendsAnySelectorOf: aCollection
]

{ #category : 'scanning' }
CleanBlockClosure >> sendsToSuper [
	"Clean blocks by definition do not access super"
	^false
]

{ #category : 'scanning' }
CleanBlockClosure >> writesField: varIndex [
	"Clean blocks by definition do not read fields"
	^false
]

{ #category : 'scanning' }
CleanBlockClosure >> writesRef: literalAssociation [
	"Clean blocks can access Literal Variables"
	^self compiledBlock writesRef: literalAssociation
]
