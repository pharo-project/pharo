Class {
	#name : 'DebugSessionExceptionTest',
	#superclass : 'DebuggerTest',
	#category : 'Debugger-Model-Tests-Core',
	#package : 'Debugger-Model-Tests',
	#tag : 'Core'
}

{ #category : 'helpers' }
DebugSessionExceptionTest >> debuggedProcessException [

	^ thisProcess
		  evaluate: [ LocalProcessException value ]
		  onBehalfOf: process
]

{ #category : 'helpers' }
DebugSessionExceptionTest >> debuggedProcessException: anObject [

	^ thisProcess
		  evaluate: [ LocalProcessException value: anObject ]
		  onBehalfOf: process
]

{ #category : 'helpers' }
DebugSessionExceptionTest >> methodWithHalt [
	<haltOrBreakpointForTesting>
	self halt.
	^1+1
]

{ #category : 'tests' }
DebugSessionExceptionTest >> testDefaultDebuggerException [

	self settingUpSessionAndProcessAndContextForBlock: [
		self methodWithHalt ].
	
	self assert: self debuggedProcessException equals: nil.
	self assert: session exception equals: nil
]

{ #category : 'tests' }
DebugSessionExceptionTest >> testProcessExceptionPrevailsOverSessionException [

	self settingUpSessionAndProcessAndContextForBlock: [
		self methodWithHalt ].

	session exception: 1.
	self assert: session exception equals: 1.
	
	self debuggedProcessException: 2.
	self assert: session exception equals: self debuggedProcessException.
	self assert: session exception equals: 2
]

{ #category : 'tests' }
DebugSessionExceptionTest >> testProcessExceptionUpdatesWhenRaisedDuringStepping [

	self settingUpSessionAndProcessAndContextForBlock: [
		self methodWithHalt ].
	
	session exception: OupsNullException new.
	session stepOver.
	self assert: session exception class equals: Halt
	
]
