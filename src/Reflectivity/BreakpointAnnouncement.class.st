Class {
	#name : #BreakpointAnnouncement,
	#superclass : #SystemAnnouncement,
	#instVars : [
		'breakpoint',
		'nodes'
	],
	#category : #'Reflectivity-Breakpoints'
}

{ #category : #'instance creation' }
BreakpointAnnouncement class >> on: aBreakpoint nodes: nodes [
	^ self new
		breakpoint: aBreakpoint;
		nodes: nodes;
		yourself
]

{ #category : #accessing }
BreakpointAnnouncement >> breakpoint [
	^ breakpoint
]

{ #category : #accessing }
BreakpointAnnouncement >> breakpoint: anObject [
	breakpoint := anObject
]

{ #category : #accessing }
BreakpointAnnouncement >> nodes [
	^ nodes
]

{ #category : #accessing }
BreakpointAnnouncement >> nodes: anObject [
	nodes := anObject
]
