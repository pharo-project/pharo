"
I'm in charge to style an ast when there are breakpoints that are enabled
"
Class {
	#name : 'BreakpointIconStyler',
	#superclass : 'IconStyler',
	#category : 'Reflectivity-Tools-Breakpoints',
	#package : 'Reflectivity-Tools',
	#tag : 'Breakpoints'
}

{ #category : 'testing' }
BreakpointIconStyler class >> isStaticStyler [

	^ true
]

{ #category : 'defaults' }
BreakpointIconStyler >> highlightColor [

	^ self theme dangerBackgroundColor alpha: 0.1
]

{ #category : 'defaults' }
BreakpointIconStyler >> iconBlock: aNode [
	 ^ [ :seg |
	   aNode breakpoints do: [ :breakpoint | breakpoint remove ].
	   seg delete ]
]

{ #category : 'defaults' }
BreakpointIconStyler >> iconFor: aNode [
	^ self iconNamed: #halt
]

{ #category : 'defaults' }
BreakpointIconStyler >> iconLabel: aNode [
	^ 'Breakpoint'
]

{ #category : 'testing' }
BreakpointIconStyler >> shouldStyleNode: aNode [
	^aNode hasBreakpoint and: [ aNode breakpoints anySatisfy: [:brkpt | brkpt isEnabled ]]
]
