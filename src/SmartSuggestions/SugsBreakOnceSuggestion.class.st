"
This class defines AST node menus for one time breakpoints
"
Class {
	#name : #SugsBreakOnceSuggestion,
	#superclass : #SugsBreakpointSuggestion,
	#category : 'SmartSuggestions-Suggestion'
}

{ #category : #execution }
SugsBreakOnceSuggestion >> execute [
	context hasUnacceptedEdits ifTrue: [ ^context sourceTextArea acceptEdits].
	
	self selectedNodeHasBreakpoint 
		ifFalse: [Breakpoint new node: context selectedNode; once; install]
		ifTrue: [ Breakpoint removeFrom: context selectedNode]
]

{ #category : #execution }
SugsBreakOnceSuggestion >> isValidForContext [
	^self selectedNodeHasBreakpoint not and: [self hasUnacceptedEdits not and:[ super isValidForContext ]]
]

{ #category : #accessing }
SugsBreakOnceSuggestion >> label [
	^ 'Break once' translated
]
