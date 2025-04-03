Class {
	#name : 'FastStepThroughTest',
	#superclass : 'StepThroughTest',
	#category : 'Debugger-Model-Tests-Core',
	#package : 'Debugger-Model-Tests',
	#tag : 'Core'
}

{ #category : 'utilities' }
FastStepThroughTest >> settingUpSessionAndProcessAndContextForBlock: block [
	super settingUpSessionAndProcessAndContextForBlock: block.
	currentFastStepThrough := DebugSession fastStepThroughMode.	
	DebugSession fastStepThroughMode: true.
	session :=  DebugSession named: 'test session' on: process startedAt: context
]

{ #category : 'tests' }
FastStepThroughTest >> testStepThroughReturnValue [
	"In a context c, define a block b, send a message to another method to get b evaluated.
	Testing that a step through on this message send moves the execution to the point where the block b is about to be evaluated."
	| node expectedMethod |
	self settingUpSessionAndProcessAndContextForBlock: [ self stepA1 ].
	session stepInto.
	session stepInto.
	"Reached node 'self evalBlockThenReturnOne: [self stepA2]' of method stepA1"
	"Checking that the execution is indeed at this node"
	self assert: (session interruptedContext method) equals: (self class lookupSelector: #stepA1).
	node := (self class lookupSelector: #stepA1) sourceNodeForPC: session interruptedContext pc.
	self assert: node isMessage.
	self assert: node receiver isSelfVariable.
	self assert: node selector equals: #evalBlockThenReturnOne:.
	session stepThrough.

	"With fullblocks the method of the suspended context is a compiledBlock, not the method having it"
	expectedMethod := (self class lookupSelector: #stepA1) literalAt: 1 .

	"Checking that after the step through, the execution is at the 'self stepA2' node of the stepA1 method"
	self assert: (session interruptedContext method) equals: expectedMethod.
	node := expectedMethod sourceNodeForPC: session interruptedContext pc.
	self assert: node isMessage.
	self assert: node receiver isSelfVariable.
	self assert: node selector equals: #stepA2.
	self deny: (session isContextPostMortem: session interruptedContext)
]
