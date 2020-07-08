Class {
	#name : #StRemoveBreakpointCommand,
	#superclass : #SpToolCommand,
	#category : #'NewTools-ObjectCentricBreakpoints'
}

{ #category : #default }
StRemoveBreakpointCommand class >> defaultName [
	^'Remove'
]

{ #category : #executing }
StRemoveBreakpointCommand >> execute [
	| breakpoint |
	breakpoint := (self transform value: self context)
		at: #breakpoint
		ifAbsent: [ ^ self inform: 'Edit your code to remove halts' ].
	self context removeBreakpoint: breakpoint
]
