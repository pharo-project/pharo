"
I am a wrapper for blocks to enable halt for the fast step through feature
"
Class {
	#name : 'HaltingBlock',
	#superclass : 'FullBlockClosure',
	#type : 'variable',
	#instVars : [
		'originalBlock'
	],
	#category : 'Debugger-Model-Core',
	#package : 'Debugger-Model',
	#tag : 'Core'
}

{ #category : 'exceptions' }
HaltingBlock >> haltIfStepping [

	<debuggerCompleteToSender>
	DebuggerSteppingState value ifTrue: [ Break break ]
]

{ #category : 'accessing' }
HaltingBlock >> home [

	^ originalBlock home
]

{ #category : 'accessing' }
HaltingBlock >> originalBlock [

	^ originalBlock
]

{ #category : 'accessing' }
HaltingBlock >> originalBlock: aBlock [

	outerContext := aBlock outerContext.
	numArgs := aBlock numArgs.
	receiver := aBlock receiver.
	compiledBlock := aBlock compiledBlock.
	originalBlock := aBlock
]

{ #category : 'accessing' }
HaltingBlock >> sender [
	self flag: 'Delete me without breaking the fast step through'.
	^ originalBlock sender
]

{ #category : 'evaluating' }
HaltingBlock >> value [

	^ self valueWithArguments: #(  )
]

{ #category : 'evaluating' }
HaltingBlock >> value: firstArg [

	^ self valueWithArguments: { firstArg }
]

{ #category : 'evaluating' }
HaltingBlock >> value: firstArg value: secondArg [

	^ self valueWithArguments: {
			  firstArg.
			  secondArg }
]

{ #category : 'evaluating' }
HaltingBlock >> value: firstArg value: secondArg value: thirdArg [

	^ self valueWithArguments: {
			  firstArg.
			  secondArg.
			  thirdArg }
]

{ #category : 'evaluating' }
HaltingBlock >> value: firstArg value: secondArg value: thirdArg value: fourthArg [

	^ self valueWithArguments: {
			  firstArg.
			  secondArg.
			  thirdArg.
			  fourthArg }
]

{ #category : 'evaluating' }
HaltingBlock >> valueNoContextSwitch [

	self haltIfStepping.
	^ originalBlock valueNoContextSwitch
]

{ #category : 'evaluating' }
HaltingBlock >> valueNoContextSwitch: anArg [

	self haltIfStepping.
	^ originalBlock valueNoContextSwitch: anArg
]

{ #category : 'evaluating' }
HaltingBlock >> valueWithArguments: anArray [

	self haltIfStepping.
	^ originalBlock valueWithArguments: anArray
]
