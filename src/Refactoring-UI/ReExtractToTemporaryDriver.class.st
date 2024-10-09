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
						 	to: newVariableName
							from: selector
							in: class
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

	| dialog |
	dialog := (self variableNameEditorPresenterClass
			openOn: variableName
			withInvalidArgs: []
			canRenameArgs: false
			canRemoveArgs: false
			canAddArgs: false
			canEditName: true ) presenter.
]

{ #category : 'execution' }
ReExtractToTemporaryDriver >> requestVariableName [

	| dialog |
	newVariableName := RBArgumentName name: ''.
	dialog := self requestDialogWith: newVariableName .
	dialog ifNil: [ shouldEscape := true. ^ self ].
	
	^ dialog variableName
]

{ #category : 'execution' }
ReExtractToTemporaryDriver >> runRefactoring [

	self configureRefactoring.
	
	refactoring prepareForExecution.
	
	newVariableName := self requestVariableName.
	shouldEscape ifTrue: [ ^ self ].
	
	self applyChanges
]

{ #category : 'accessing' }
ReExtractToTemporaryDriver >> scopes: aCollection [ 
	scopes := aCollection.
	model :=  self refactoringScopeOn: scopes first
]

{ #category : 'validation' }
ReExtractToTemporaryDriver >> validateName: aName onPresenter: presenter [
 
	| firstCondition secondCondition result firstStream secondStream errorMessages |
   	firstCondition := refactoring preconditionValidInstVarName.
	secondCondition := refactoring preconditionCheckVariableName.
	firstStream := WriteStream on: String new.
	secondStream := WriteStream on: String new.
	
	firstCondition check ifFalse: [ 
        firstCondition violationMessageOn: firstStream ].
	secondCondition check ifFalse: [ 
        secondCondition violationMessageOn: secondStream ].
	errorMessages := firstStream contents.
	errorMessages := errorMessages , (errorMessages isEmpty
							ifFalse: [ '. ', secondStream contents ]
							ifTrue: [ secondStream contents ]).
	
	result := (firstCondition check and: [ secondCondition check ]).
    
   result 
		ifFalse: [
      		shouldEscape := true.
			presenter setErrorText: errorMessages ]
       ifTrue: [ presenter setErrorText: '' ].
   
	^ result.
]

{ #category : 'accessing' }
ReExtractToTemporaryDriver >> variableNameEditorPresenterClass [
    ^ variableNameEditorPresenterClass ifNil: [ variableNameEditorPresenterClass := StMethodNameEditorPresenter]

]

{ #category : 'accessing' }
ReExtractToTemporaryDriver >> variableNameEditorPresenterClass: aClass [
	variableNameEditorPresenterClass := aClass
]
