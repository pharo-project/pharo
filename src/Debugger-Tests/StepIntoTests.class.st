Class {
	#name : #StepIntoTests,
	#superclass : #DebuggerTests,
	#category : #'Debugger-Tests'
}

{ #category : #helper }
StepIntoTests >> callQuickMethodWithoutReturningItsResult [
	self quickMethod.
]

{ #category : #helper }
StepIntoTests >> quickMethod [
	^1.
]

{ #category : #helper }
StepIntoTests >> returnQuickMethodResult [
	^ self quickMethod.
]

{ #category : #helper }
StepIntoTests >> stepA1 [
	^42
]

{ #category : #tests }
StepIntoTests >> testDetectionOfExecutionEnd [
	self settingUpSessionAndProcessAndContextForBlock: [  ].
	session stepInto.
	"These ways do not detect that the execution ended"
	self assert: session interruptedContext isDead not.
	self assert: (session isContextPostMortem: (session interruptedContext)) not.
	self assert: session interruptedProcess suspendedContext isNil not.
	self assert: session interruptedProcess suspendedContext isDead not.
	"This way seems to detect that an execution ended"
	self assert: session interruptedProcess isTerminated.

]

{ #category : #tests }
StepIntoTests >> testStepIntoDeadContextShouldRaiseException [
	self settingUpSessionAndProcessAndContextForBlock: [  ].
	session stepInto. "The first step into executes the (empty) body of the block and returns from it. From this point on, the context is dead since it returned."
	
	"We should be able to call stepInto.
	However, calling it provokes infinite exceptions and non recuperable images."
	"session stepInto."
	
	"We have found that the debugSession calls stepToSendOrReturn after step: which could be the cause of the issue.
	This test is green if we avoid calling that part and just call step:"
	session interruptedProcess step: session interruptedContext.
	
	self assert: context isDead
]

{ #category : #tests }
StepIntoTests >> testStepIntoMethodCallShouldActivateIt [
	"Stepping into a method call should create a context for it at the top of the context stack"
	self settingUpSessionAndProcessAndContextForBlock: [ self stepA1].
	session stepInto.
	session stepInto.
	self
		assert: process suspendedContext method
		equals: self class >> #stepA1
]

{ #category : #tests }
StepIntoTests >> testStepIntoQuickMethodCallNotReturnedShouldLeaveTheValueStackEmpty [
	"Stepping into a quick method whose result is not used should leave the value stack empty"
	self settingUpSessionAndProcessAndContextForBlock: [ self callQuickMethodWithoutReturningItsResult].
	session stepInto.
	session stepInto.
	session stepInto.
	self
		assert: process suspendedContext method
		equals: self class >> #callQuickMethodWithoutReturningItsResult.
	self
		assert: process suspendedContext size
		equals: 0.
]

{ #category : #tests }
StepIntoTests >> testStepIntoQuickMethodCallReturnedShouldPushReturnValueToTheStack [
	"Stepping into a call to a quick method whose result is used should push its return value to the stack"
	self settingUpSessionAndProcessAndContextForBlock: [ self returnQuickMethodResult].
	session stepInto.
	session stepInto.
	session stepInto.
	self
		assert: process suspendedContext method
		equals: self class >> #returnQuickMethodResult.
	self
		assert: process suspendedContext top
		equals: 1.
]
