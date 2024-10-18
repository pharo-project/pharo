Class {
	#name : 'DebugSessionExceptionTest',
	#superclass : 'DebuggerTest',
	#category : 'Debugger-Model-Tests-Core',
	#package : 'Debugger-Model-Tests',
	#tag : 'Core'
}

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
	
	self assert: LocalProcessException value equals: nil.
	self assert: session exception equals: nil
]

{ #category : 'tests' }
DebugSessionExceptionTest >> testProcessExceptionPrevailsOverSessionException [

	self settingUpSessionAndProcessAndContextForBlock: [
		self methodWithHalt ].
	
	session exception: 1.
	self assert: session exception equals: 1.
	
	LocalProcessException value: 2.
	self assert: session exception equals: LocalProcessException value.
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
