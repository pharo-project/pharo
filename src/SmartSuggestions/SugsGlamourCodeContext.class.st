"
I know how to use a Glamour  code model to  and extract the information needed for the suggestions
"
Class {
	#name : #SugsGlamourCodeContext,
	#superclass : #SugsAbstractContext,
	#category : #'SmartSuggestions-Menu'
}

{ #category : #selection }
SugsGlamourCodeContext >> selectTheTextForTheNode [
	selectionPreviousHighligth := self model primarySelectionInterval .
	selectedNode ifNotNil: [ self model primarySelectionInterval: (self selectedNode start to: self selectedNode stop)]
]

{ #category : #selection }
SugsGlamourCodeContext >> selectedClass [
	^ model selectedBehavior
]

{ #category : #selection }
SugsGlamourCodeContext >> selectedInterval [
	^ self model glamourPresentation selectionInterval
]

{ #category : #selection }
SugsGlamourCodeContext >> selectedMethod [
	^ model selectedMethod
]

{ #category : #refactoring }
SugsGlamourCodeContext >> sourceTextArea [ 
	^ model dependents ifEmpty: [ RubScrolledTextModel new interactionModel: model  ] ifNotEmpty: [ model dependents at: 1  ]
]
