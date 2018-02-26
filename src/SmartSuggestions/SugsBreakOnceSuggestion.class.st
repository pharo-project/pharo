"
This class defines AST node menus for one time breakpoints
"
Class {
	#name : #SugsBreakOnceSuggestion,
	#superclass : #SugsBreakpointSuggestion,
	#category : #'SmartSuggestions-Suggestion'
}

{ #category : #accessing }
SugsBreakOnceSuggestion class >> nodes [
	^{RBProgramNode}
]

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

{ #category : #display }
SugsBreakOnceSuggestion >> keyText [
	^'b,o'
]

{ #category : #display }
SugsBreakOnceSuggestion >> label [
	^ 'Break once' translated
]

{ #category : #display }
SugsBreakOnceSuggestion >> position [
	^16
]
