"
I'm in charge to style an ast when there are breakpoints
"
Class {
	#name : #BreakpointIconStyler,
	#superclass : #IconStyler,
	#category : #Reflectivity-Tools-Breakpoints
}

{ #category : #defaults }
BreakpointIconStyler >> highlightColor [
	^(Color red alpha: 0.1)
]

{ #category : #defaults }
BreakpointIconStyler >> iconBlock: aNode [
	^[ :seg | 
		Breakpoint removeFrom: aNode.
		seg delete]
]

{ #category : #defaults }
BreakpointIconStyler >> iconFor: aNode [
	^ self iconNamed: #haltIcon 
]

{ #category : #defaults }
BreakpointIconStyler >> iconLabel: aNode [
	^ 'Breakpoint'
]

{ #category : #testing }
BreakpointIconStyler >> shouldStyleNode: aNode [
	^aNode hasBreakpoint
]
