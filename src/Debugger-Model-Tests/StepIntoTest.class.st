Class {
	#name : 'StepIntoTest',
	#superclass : 'DebuggerTest',
	#category : 'Debugger-Model-Tests-Core',
	#package : 'Debugger-Model-Tests',
	#tag : 'Core'
}

{ #category : 'helper' }
StepIntoTest >> callQuickMethodWithoutReturningItsResult [
	self quickMethod
]

{ #category : 'helper' }
StepIntoTest >> quickMethod [
	^1
]

{ #category : 'helper' }
StepIntoTest >> returnQuickMethodResult [
	^ self quickMethod
]

{ #category : 'helper' }
StepIntoTest >> stepA1 [
	^42
]

{ #category : 'tests' }
StepIntoTest >> testStepIntoDeadContext [
	self settingUpSessionAndProcessAndContextForBlock: [ self returnQuickMethodResult ].
	"we step till the block returns"
	session stepInto.
	session stepInto.
	session stepInto.
	session stepInto.
	self assert: context isDead.
	"Further steps are ignored, see DebugSession>>#stepInto:"
	session stepInto
]

{ #category : 'tests' }
StepIntoTest >> testStepIntoDoIt [

	| method node |
	method := nil compiler
		          isScripting: true;
		          compile: '| a b | a := 40. b := a + 2. ^ b'.
	self settingUpSessionAndProcessAndContextForBlock: [
		nil executeMethod: method ].
	[ session interruptedContext compiledCode = method ] whileFalse: [
		session stepInto ].

	node := method sourceNodeForPC: session interruptedContext pc.
	self assert: node isAssignment.
	self assert: node variable name equals: #a.

	session stepInto.

	node := method sourceNodeForPC: session interruptedContext pc.
	self assert: node isMessage.
	self assert: node selector equals: #+.

	session stepInto.

	node := method sourceNodeForPC: session interruptedContext pc.
	self assert: node isAssignment.
	self assert: node variable name equals: #b.

	session stepInto.

	node := method sourceNodeForPC: session interruptedContext pc.
	self assert: node isReturn.

	session stepInto.

	self deny: session interruptedContext method equals: method.

	[ session interruptedProcess isTerminated ] whileFalse: [
		session stepInto ].
	self assert: session interruptedProcess isTerminated
]

{ #category : 'tests' }
StepIntoTest >> testStepIntoMethodCallShouldActivateIt [
	"Stepping into a method call should create a context for it at the top of the context stack"
	self settingUpSessionAndProcessAndContextForBlock: [ self stepA1 ].
	[session interruptedContext method == (self class>>#stepA1)]
		whileFalse: [ session stepInto ].

	self
		assert: process suspendedContext method
		equals: self class >> #stepA1
]

{ #category : 'tests' }
StepIntoTest >> testStepIntoQuickMethodCallNotReturnedShouldLeaveTheValueStackEmpty [
	"Stepping into a quick method whose result is not used should leave the value stack empty"
	self settingUpSessionAndProcessAndContextForBlock: [ self callQuickMethodWithoutReturningItsResult].
	[session interruptedContext method == (self class>>#callQuickMethodWithoutReturningItsResult)]
		whileFalse: [ session stepInto ].

	session stepInto.
	self
		assert: process suspendedContext method
		equals: self class >> #callQuickMethodWithoutReturningItsResult.
	self
		assert: process suspendedContext size
		equals: 0
]

{ #category : 'tests' }
StepIntoTest >> testStepIntoQuickMethodCallReturnedShouldPushReturnValueToTheStack [
	"Stepping into a call to a quick method whose result is used should push its return value to the stack"
	self settingUpSessionAndProcessAndContextForBlock: [ self returnQuickMethodResult].
	[session interruptedContext method == (self class>>#returnQuickMethodResult)]
		whileFalse: [ session stepInto ].

	session stepInto.
	self
		assert: process suspendedContext method
		equals: self class >> #returnQuickMethodResult.
	self
		assert: process suspendedContext top
		equals: 1
]

{ #category : 'tests' }
StepIntoTest >> testStepIntoUntilTermination [
	"Stepping until termination."
	self settingUpSessionAndProcessAndContextForBlock: [ 1+2 ].

	[ session interruptedProcess isTerminated ] whileFalse: [ session stepInto ].

	self assert: session interruptedProcess isTerminated
]

{ #category : 'tests' }
StepIntoTest >> testStepIntoUntilTerminationClean [
	"Stepping until termination."
	self settingUpSessionAndProcessAndContextForCleanBlock: [ ].

	[ session interruptedProcess isTerminated ] whileFalse: [ session stepInto ].

	self assert: session interruptedProcess isTerminated
]
