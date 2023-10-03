"
I'm in charge to style an ast when there are breakpoints that are disabled
"
Class {
	#name : 'DisabledBreakpointIconStyler',
	#superclass : 'IconStyler',
	#category : 'Reflectivity-Tools-Breakpoints',
	#package : 'Reflectivity-Tools',
	#tag : 'Breakpoints'
}

{ #category : 'testing' }
DisabledBreakpointIconStyler class >> isStaticStyler [

	^ true
]

{ #category : 'defaults' }
DisabledBreakpointIconStyler >> highlightColor [
	^(Color darkGray alpha: 0.1)
]

{ #category : 'defaults' }
DisabledBreakpointIconStyler >> iconBlock: aNode [
	 ^ [ :seg |
	   aNode breakpoints do: [ :breakpoint | breakpoint enable ].
	   seg delete ]
]

{ #category : 'defaults' }
DisabledBreakpointIconStyler >> iconFor: aNode [
	^ self iconNamed: #smallInfo
]

{ #category : 'defaults' }
DisabledBreakpointIconStyler >> iconLabel: aNode [
	^ 'Breakpoint (disabled)'
]

{ #category : 'testing' }
DisabledBreakpointIconStyler >> shouldStyleNode: aNode [
	^aNode hasBreakpoint and: [ aNode breakpoints noneSatisfy: [:brkpt | brkpt isEnabled ]]
]
