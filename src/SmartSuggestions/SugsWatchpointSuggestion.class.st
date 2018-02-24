"
This class defines AST node menus for watchpoints
"
Class {
	#name : #SugsWatchpointSuggestion,
	#superclass : #SugsSuggestion,
	#category : #'SmartSuggestions-Suggestion'
}

{ #category : #accessing }
SugsWatchpointSuggestion class >> nodes [
	^{RBMethodNode . RBMessageNode . RBAssignmentNode . RBVariableNode }
]

{ #category : #execution }
SugsWatchpointSuggestion >> execute [
	context ifNil: [ ^self ].
	context hasUnacceptedEdits ifTrue: [ ^context sourceTextArea acceptEdits].
	
	context selectedNode hasWatchpoint
		ifFalse: [ Watchpoint in: context selectedNode ]
		ifTrue: [ Watchpoint removeFrom: context selectedNode]
]

{ #category : #display }
SugsWatchpointSuggestion >> icon [
	^self iconNamed: #haloView
]

{ #category : #execution }
SugsWatchpointSuggestion >> isValidForContext [
	context ifNil: [ ^false ].
	^context hasUnacceptedEdits not
	
]

{ #category : #display }
SugsWatchpointSuggestion >> label [
	^self selectedNodeHasWatchpoint
		ifFalse: [ 'Add Watchpoint' translated ]
		ifTrue: [ 'Remove Watchpoint' translated ]
	
]

{ #category : #display }
SugsWatchpointSuggestion >> position [
	^18
]

{ #category : #'private - testing' }
SugsWatchpointSuggestion >> selectedNodeHasWatchpoint [
	context ifNil: [ ^false ].
	^context selectedNode hasWatchpoint
]
