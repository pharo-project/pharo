"
This class is an abstrat class for defining breakpoint commands
"
Class {
	#name : #SugsBreakpointSuggestion,
	#superclass : #SugsSuggestion,
	#category : 'SmartSuggestions-Suggestion'
}

{ #category : #breakpoints }
SugsBreakpointSuggestion >> hasUnacceptedEdits [
	context ifNil: [ ^false ].
	^context hasUnacceptedEdits
	
]

{ #category : #accessing }
SugsBreakpointSuggestion >> label [
	self subclassResponsibility
]

{ #category : #breakpoints }
SugsBreakpointSuggestion >> selectedNodeHasBreakpoint [
	context ifNil: [ ^false ].
	^ context selectedNode hasBreakpoint
]
