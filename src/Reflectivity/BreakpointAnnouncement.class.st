Class {
	#name : 'BreakpointAnnouncement',
	#superclass : 'SystemAnnouncement',
	#instVars : [
		'breakpoint',
		'nodes'
	],
	#category : 'Reflectivity-Breakpoints',
	#package : 'Reflectivity',
	#tag : 'Breakpoints'
}

{ #category : 'instance creation' }
BreakpointAnnouncement class >> on: aBreakpoint nodes: nodes [
	^ self new
		breakpoint: aBreakpoint;
		nodes: nodes;
		yourself
]

{ #category : 'accessing' }
BreakpointAnnouncement >> affectsMethod: aMethod [

	^ self link methods anySatisfy: [:each | each compiledMethod == aMethod compiledMethod ]
]

{ #category : 'accessing' }
BreakpointAnnouncement >> affectsMethods [
	^true
]

{ #category : 'accessing' }
BreakpointAnnouncement >> affectsMethodsDefinedInClass: aClass [
	^ self link methods
		anySatisfy: [ :each | each methodClass isNotNil and: [ each origin == aClass ] ]
]

{ #category : 'accessing' }
BreakpointAnnouncement >> affectsMethodsDefinedInPackage: aPackage [
	^ self link methods
		anySatisfy:
			[ :each | each methodClass isNotNil and: [ each package == aPackage ] ]
]

{ #category : 'accessing' }
BreakpointAnnouncement >> breakpoint [
	^ breakpoint
]

{ #category : 'accessing' }
BreakpointAnnouncement >> breakpoint: anObject [
	breakpoint := anObject
]

{ #category : 'accessing' }
BreakpointAnnouncement >> link [
	^ self breakpoint link
]

{ #category : 'accessing' }
BreakpointAnnouncement >> nodes [
	^ nodes
]

{ #category : 'accessing' }
BreakpointAnnouncement >> nodes: anObject [
	nodes := anObject
]
