"
This class defines AST node menus for breakpoints
"
Class {
	#name : #SugsBreakAlwaysSuggestion,
	#superclass : #SugsBreakpointSuggestion,
	#category : #'SmartSuggestions-Suggestion'
}

{ #category : #execution }
SugsBreakAlwaysSuggestion >> execute [
	context hasUnacceptedEdits ifTrue: [ ^context sourceTextArea acceptEdits].
	
	self selectedNodeHasBreakpoint 
		ifFalse: [Breakpoint new node: context selectedNode; always; install]
		ifTrue: [ Breakpoint removeFrom: context selectedNode]
]

{ #category : #display }
SugsBreakAlwaysSuggestion >> keyText [
	^'b,a'
]

{ #category : #display }
SugsBreakAlwaysSuggestion >> label [
	self hasUnacceptedEdits ifTrue: [ ^'Accept to enable breakpoints' ].

	^self selectedNodeHasBreakpoint
		ifFalse: [ 'Break' translated ]
		ifTrue: [ 'Remove breakpoint' translated ]
]

{ #category : #display }
SugsBreakAlwaysSuggestion >> position [
	^15
]
