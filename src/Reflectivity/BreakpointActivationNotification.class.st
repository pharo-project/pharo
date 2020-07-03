"
A trivial breakpoint activation notification. 
"
Class {
	#name : #BreakpointActivationNotification,
	#superclass : #BreakpointNotification,
	#instVars : [
		'activated'
	],
	#category : #'Reflectivity-Breakpoints'
}

{ #category : #accessing }
BreakpointActivationNotification >> activated [
	^ activated
]

{ #category : #accessing }
BreakpointActivationNotification >> activated: anObject [
	activated := anObject
]

{ #category : #initialize }
BreakpointActivationNotification >> initialize [ 	
	super initialize.
	activated := true
]
