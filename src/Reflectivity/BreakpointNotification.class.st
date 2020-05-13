"
Trivial abtract breakpoint notification.
I hold the affected breakpoint and a collection of nodes on which it is installed or removed from.
"
Class {
	#name : #BreakpointNotification,
	#superclass : #Object,
	#instVars : [
		'breakpoint',
		'nodes'
	],
	#category : #'Reflectivity-Breakpoints'
}

{ #category : #'instance creation' }
BreakpointNotification class >> on: aBreakpoint nodes: nodes [
	^ self new
		breakpoint: aBreakpoint;
		nodes: nodes;
		yourself
]

{ #category : #accessing }
BreakpointNotification >> breakpoint [
	^ breakpoint
]

{ #category : #accessing }
BreakpointNotification >> breakpoint: anObject [
	breakpoint := anObject
]

{ #category : #accessing }
BreakpointNotification >> nodes [
	^ nodes
]

{ #category : #accessing }
BreakpointNotification >> nodes: anObject [
	nodes := anObject
]
