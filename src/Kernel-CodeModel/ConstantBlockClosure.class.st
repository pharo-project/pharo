"
Some blocks just return a literal, without sending messages or accessing the arguments

	[] -> returns nil
	[1]
	[: arg | 'a string']

it is clear that these blocks are clean. But they are even more interesting: we can implement a special kind of Clean Block that can be evaluated faster. 

We create a dummy compiledBlock which and use it to find the pcInOuter / do pc-ast mapping.

the RBBlockNode has two methods: #isConstant returns true, #constantValue returns the literal for constant blocks.

This class implements 0-arg blocks, with correct error handling for args >0. Subclasses for 1-3 args are implemented.


	
"
Class {
	#name : 'ConstantBlockClosure',
	#superclass : 'CleanBlockClosure',
	#type : 'variable',
	#category : 'Kernel-CodeModel-Methods',
	#package : 'Kernel-CodeModel',
	#tag : 'Methods'
}

{ #category : 'instance creation' }
ConstantBlockClosure class >> numArgs: numArgs literal: aLiteral [
	| classToUse |
	classToUse := numArgs caseOf:
	{
		[0]->[ self ].
		[1]->[ ConstantBlockClosure1Arg ].
		[2]->[ ConstantBlockClosure2Arg ].
		[3]->[ ConstantBlockClosure3Arg ]
	} otherwise: [self error: 'shoud not be called with more than 3 args'].
	^ classToUse new
		numArgs: numArgs;
		literal: aLiteral
]

{ #category : 'testing' }
ConstantBlockClosure >> isConstant [

	^ true
]

{ #category : 'accessing' }
ConstantBlockClosure >> literal: anObject [
	 literal := anObject
]

{ #category : 'evaluating' }
ConstantBlockClosure >> value [
	^literal
]

{ #category : 'evaluating' }
ConstantBlockClosure >> value: anObject [
	self numArgsError: 1
]

{ #category : 'evaluating' }
ConstantBlockClosure >> value: firstArg value: secondArg [
	self numArgsError: 2
]

{ #category : 'evaluating' }
ConstantBlockClosure >> value: firstArg value: secondArg value: thirdArg [
	self numArgsError: 3
]

{ #category : 'evaluating' }
ConstantBlockClosure >> value: firstArg value: secondArg value: thirdArg value: fourthArg [
	self numArgsError: 4
]

{ #category : 'evaluating' }
ConstantBlockClosure >> valueWithArguments: anArray [
	(anArray size ~= 0) ifTrue: [self numArgsError: anArray size].
	^literal
]
