Class {
	#name : #InstructionStreamTest,
	#superclass : #TestCase,
	#instVars : [
		'operations'
	],
	#category : #'Kernel-Tests-Extended-Methods'
}

{ #category : #testing }
InstructionStreamTest >> classUnderTest [ 

	^ ClassUsedInInstructionStreamTest
]

{ #category : #testing }
InstructionStreamTest >> directedSuperSend: aString numArgs: anInteger [ 
	
	operations add: { #directSuperSend:numArgs:. aString. anInteger }
]

{ #category : #testing }
InstructionStreamTest >> pushConstant: anInteger [ 

	operations add: { #pushConstant. anInteger }
]

{ #category : #testing }
InstructionStreamTest >> pushLiteralVariable: aLiteralVariable [ 

	operations add: { #pushConstant. aLiteralVariable }
]

{ #category : #testing }
InstructionStreamTest >> pushReceiver [
	
	operations add: {#pushReceiver}
]

{ #category : #testing }
InstructionStreamTest >> send: aString super: aBoolean numArgs: anInteger [ 

	operations add: { #send:super:numArgs:. aString. aBoolean. anInteger }
]

{ #category : #running }
InstructionStreamTest >> setUp [ 

	super setUp.
	operations := OrderedCollection new
]

{ #category : #tests }
InstructionStreamTest >> testBlockWithASuperSendHasCorrectNumberOfArguments [

	| aMethod aCompiledBlock aStream |
	aMethod := self classUnderTest >> #methodWithASuperBlock.
	aCompiledBlock := aMethod literals at:1.
	
	self assert: aCompiledBlock isCompiledBlock.
	
	aStream := InstructionStream on: aCompiledBlock.

	[ aCompiledBlock endPC > aStream pc ]
		whileTrue: [aStream interpretNextInstructionFor: self].
		
	self assert: operations last equals: { #directSuperSend:numArgs:. #aSuperMethod:with:. 2 }
]

{ #category : #tests }
InstructionStreamTest >> testBlockWithASuperWithoutArgumentsSendHasCorrectNumberOfArguments [

	| aMethod aCompiledBlock aStream |
	aMethod := self classUnderTest >> #methodWithASuperBlockWithoutArguments.
	aCompiledBlock := aMethod literals at:1.
	
	self assert: aCompiledBlock isCompiledBlock.
	
	aStream := InstructionStream on: aCompiledBlock.

	[ aCompiledBlock endPC > aStream pc ]
		whileTrue: [aStream interpretNextInstructionFor: self].
		
	self assert: operations last equals: { #directSuperSend:numArgs:. #yourself. 0 }
]

{ #category : #example }
InstructionStreamTest >> testSteppingSendsDirectSend [

	| initialContext aContext receiver |
	
	receiver := self classUnderTest new.
	
	initialContext := Context 
		sender: nil receiver: receiver 
		method: self classUnderTest >> #valueOfBlockWithSupersend 
		arguments: #().

	aContext := initialContext.

	[aContext = nil] 
		whileFalse: [aContext := aContext step].
		
	self assert: receiver expectedValue equals: 42
]
