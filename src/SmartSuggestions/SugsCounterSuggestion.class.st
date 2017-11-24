"
I am creating Counters on AST nodes
"
Class {
	#name : #SugsCounterSuggestion,
	#superclass : #SugsSuggestion,
	#category : #'SmartSuggestions-Suggestion'
}

{ #category : #execution }
SugsCounterSuggestion >> execute [
	context ifNil: [ ^self ].
	context hasUnacceptedEdits ifTrue: [ ^context sourceTextArea acceptEdits].
	
	context selectedNode hasExecutionCounter
		ifFalse: [ ExecutionCounter installOn: context selectedNode]
		ifTrue: [ ExecutionCounter removeFrom: context selectedNode]
]

{ #category : #execution }
SugsCounterSuggestion >> isValidForContext [
	context ifNil: [ ^false ].
	^context hasUnacceptedEdits not
	
]

{ #category : #accessing }
SugsCounterSuggestion >> label [
	^self selectedNodeHasExecutionCounter
		ifFalse: [ 'Add Counter' translated ]
		ifTrue: [ 'Remove Counter' translated ]
	
]

{ #category : #execution }
SugsCounterSuggestion >> selectedNodeHasExecutionCounter [
	context ifNil: [ ^false ].
	^context selectedNode hasExecutionCounter
]
