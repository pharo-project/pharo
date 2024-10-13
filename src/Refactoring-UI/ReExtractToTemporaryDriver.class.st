Class {
	#name : 'ReExtractToTemporaryDriver',
	#superclass : 'RBInteractionDriver',
	#instVars : [
		'sourceInterval',
		'newVariableName',
		'selector',
		'class',
		'shouldEscape',
		'variableNameEditorPresenterClass'
	],
	#category : 'Refactoring-UI-Drivers',
	#package : 'Refactoring-UI',
	#tag : 'Drivers'
}

{ #category : 'execution' }
ReExtractToTemporaryDriver >> changes [
	
	^ refactoring privateTransform; changes

]

{ #category : 'resources' }
ReExtractToTemporaryDriver >> configureRefactoring [

	refactoring := RBExtractToTemporaryRefactoring 
							extract: sourceInterval
							from: selector
							in: class
]

{ #category : 'instance creation' }
ReExtractToTemporaryDriver >> extract: anInterval from: aSelector in: aClass [

	class := RBNamespace new classObjectFor: aClass.
	selector := aSelector.
	sourceInterval := anInterval
]

{ #category : 'instance creation' }
ReExtractToTemporaryDriver >> extract: anInterval to: aString from: aSelector in: aClass [

	class := RBNamespace new classObjectFor: aClass.
	selector := aSelector.
	sourceInterval := anInterval.
	newVariableName := aString
]

{ #category : 'initialization' }
ReExtractToTemporaryDriver >> initialize [ 
	
	super initialize.
	shouldEscape := false.
]

{ #category : 'accessing' }
ReExtractToTemporaryDriver >> requestDialogWith: variableName [

	^ requestDialog := (self variableNameEditorPresenterClass
			openOn: variableName ) presenter.
]

{ #category : 'execution' }
ReExtractToTemporaryDriver >> requestVariableName [

	| dialog variableName|
	variableName := RBArgumentName name: 'newVariableName'.
	dialog := self requestDialogWith: variableName.
	dialog ifNil: [ shouldEscape := true. ^ self ].
	
	^ dialog variableName
]

{ #category : 'execution' }
ReExtractToTemporaryDriver >> runRefactoring [

	self configureRefactoring.
	
	refactoring prepareForExecution.
	
	newVariableName := self requestVariableName.
	refactoring extract: sourceInterval to: newVariableName from: selector in: class.
	shouldEscape ifTrue: [ ^ self ].
	refactoring preconditionValidInstVarName check ifFalse: [ ^ self].
	refactoring preconditionCheckVariableName check ifFalse: [ ^ self ].
	
	self applyChanges
]

{ #category : 'accessing' }
ReExtractToTemporaryDriver >> scopes: aCollection [ 
	scopes := aCollection.
	model :=  self refactoringScopeOn: scopes first
]

{ #category : 'accessing' }
ReExtractToTemporaryDriver >> variableNameEditorPresenterClass [
    ^ variableNameEditorPresenterClass ifNil: [ variableNameEditorPresenterClass := StVariableNameEditorPresenter ]

]

{ #category : 'accessing' }
ReExtractToTemporaryDriver >> variableNameEditorPresenterClass: aClass [
	variableNameEditorPresenterClass := aClass
]
