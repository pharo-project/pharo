"
This class defines AST node menus for watchpoints
"
Class {
	#name : #SugsWatchpointSuggestion,
	#superclass : #SugsSuggestion,
	#category : #'SmartSuggestions-Suggestion'
}

{ #category : #execution }
SugsWatchpointSuggestion >> execute [
	context ifNil: [ ^self ].
	context hasUnacceptedEdits ifTrue: [ ^context sourceTextArea acceptEdits].
	
	context selectedNode hasWatchpoint
		ifFalse: [ Watchpoint in: context selectedNode ]
		ifTrue: [ Watchpoint removeFrom: context selectedNode]
]

{ #category : #execution }
SugsWatchpointSuggestion >> isValidForContext [
	context ifNil: [ ^false ].
	^context hasUnacceptedEdits not
	
]

{ #category : #accessing }
SugsWatchpointSuggestion >> label [
	^self selectedNodeHasWatchpoint
		ifFalse: [ 'Add Watchpoint' translated ]
		ifTrue: [ 'Remove Watchpoint' translated ]
	
]

{ #category : #'private - testing' }
SugsWatchpointSuggestion >> selectedNodeHasWatchpoint [
	context ifNil: [ ^false ].
	^context selectedNode hasWatchpoint
]
