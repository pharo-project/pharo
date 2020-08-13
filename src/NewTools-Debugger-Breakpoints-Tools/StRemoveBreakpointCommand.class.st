"
Removes a breakpoint from a selected breakpoint item in a StObjectBreakpointInspection. 
"
Class {
	#name : #StRemoveBreakpointCommand,
	#superclass : #SpToolCommand,
	#category : #'NewTools-Debugger-Breakpoints-Tools-Model'
}

{ #category : #default }
StRemoveBreakpointCommand class >> defaultName [
	^'Remove'
]

{ #category : #executing }
StRemoveBreakpointCommand >> execute [

	(self transform value: self context) removeInPresenterContext:
		self context
]
