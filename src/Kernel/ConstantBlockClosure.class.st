"
Some blocks just return a literal, without sending messages or accessing the arguments

	[] -> returns nil
	[1]
	[: arg | 'a string']

it is clear that these blocks are clean. But they are even more interesting: we can implement a special kind of Clean Block that can be evaluated faster. 

the RBBlockNode has two methods: #isConstant returns true, #constantValue returns the literal for constant blocks.
"
Class {
	#name : #ConstantBlockClosure,
	#superclass : #CleanBlockClosure,
	#type : #variable,
	#category : #'Kernel-Methods'
}

{ #category : #accessing }
ConstantBlockClosure >> returnValue: anObject [
	 literal := anObject.
	"dummy compiledBlock, we use it to find the pcInOuter 
	and for queries that are done in the bytecode level."
	compiledBlock := 
		(IRBuilder new 
			pushLiteral: literal; 
			blockReturnTop; ir) compiledBlock
]

{ #category : #evaluating }
ConstantBlockClosure >> value [
	^literal
]

{ #category : #evaluating }
ConstantBlockClosure >> value: anObject [
	^literal
]

{ #category : #evaluating }
ConstantBlockClosure >> value: firstArg value: secondArg [
	^literal
]

{ #category : #evaluating }
ConstantBlockClosure >> value: firstArg value: secondArg value: thirdArg [
	^literal
]

{ #category : #evaluating }
ConstantBlockClosure >> value: firstArg value: secondArg value: thirdArg value: fourthArg [
	^literal
]

{ #category : #evaluating }
ConstantBlockClosure >> valueWithArguments: anArray [
	^literal
]
