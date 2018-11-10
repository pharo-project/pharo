"
I return all methods which have installed breakpoints or send the halt message (and friends like haltIf:)
"
Class {
	#name : #ClyActiveBreakpoints,
	#superclass : #ClyMethodQuery,
	#category : #'Calypso-SystemPlugins-Reflectivity-Queries-Breakpoints'
}

{ #category : #printing }
ClyActiveBreakpoints >> description [
	^'active breakpoints'
]

{ #category : #testing }
ClyActiveBreakpoints >> selectsMethod: aMethod [
	^aMethod hasBreakpoint or: [ aMethod containsHalt ]
]
