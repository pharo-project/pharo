"
I am a notification sent to Breakpoint observers when a breakpoint is hit.
I can hold a value from the execution context the breakpoint was hit. 
"
Class {
	#name : #BreakpointHitNotification,
	#superclass : #BreakpointNotification,
	#instVars : [
		'valueOrNil'
	],
	#category : #'Reflectivity-Breakpoints'
}

{ #category : #accessing }
BreakpointHitNotification >> valueOrNil [
	^ valueOrNil
]

{ #category : #accessing }
BreakpointHitNotification >> valueOrNil: anObject [
	valueOrNil := anObject
]
