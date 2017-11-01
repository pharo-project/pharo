"
This class defines AST node menus for conditional  breakpoints
"
Class {
	#name : #SugsBreakConditionSuggestion,
	#superclass : #SugsBreakpointSuggestion,
	#category : 'SmartSuggestions-Suggestion'
}

{ #category : #breakpoints }
SugsBreakConditionSuggestion >> breakpointForCondition [
	(UIManager default request: 'Break on what condition?')	
		ifNotNil: [ :condition | Breakpoint new 
			node: context selectedNode;
			condition: (Smalltalk compiler evaluate: condition);
			install]
]

{ #category : #execution }
SugsBreakConditionSuggestion >> execute [
	context hasUnacceptedEdits ifTrue: [ ^context sourceTextArea acceptEdits].
	
	self selectedNodeHasBreakpoint 
		ifFalse: [ self breakpointForCondition ]
		ifTrue: [ Breakpoint removeFrom: context selectedNode]
]

{ #category : #execution }
SugsBreakConditionSuggestion >> isValidForContext [
	^self selectedNodeHasBreakpoint not and: [self hasUnacceptedEdits not and:[ super isValidForContext ]]
]

{ #category : #accessing }
SugsBreakConditionSuggestion >> label [
	^'Break on condition' translated
]
