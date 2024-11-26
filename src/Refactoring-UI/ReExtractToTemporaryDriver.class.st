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
							model: model
							extract: sourceInterval
							from: selector
							in: class
]

{ #category : 'instance creation' }
ReExtractToTemporaryDriver >> extract: anInterval from: aSelector in: aClass [

	class := RBNamespace new classObjectFor: aClass.
	selector := aSelector.
	sourceInterval := anInterval.
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
	variableName := 'enter a variable name'.
	dialog := self requestDialogWith: variableName.
	dialog ifNil: [ shouldEscape := true. ^ self].
	^ dialog
]

{ #category : 'execution' }
ReExtractToTemporaryDriver >> runRefactoring [

	| checkVariableName checkVariableDefinition messageStream dialog |
	
	self configureRefactoring.
   refactoring prepareForExecution.
    
   [ 
		dialog := self requestVariableName.
		newVariableName := dialog variableName.
		shouldEscape ifTrue: [ ^ self ].
		refactoring extractToVariable: newVariableName.
		
		checkVariableName := refactoring preconditionValidInstVarName.
		checkVariableDefinition := refactoring preconditionCheckVariableName.
      messageStream := WriteStream on: String new.

     	checkVariableName := refactoring preconditionValidInstVarName.
      checkVariableName check ifFalse: [
     		checkVariableName violationMessageOn: messageStream.
          dialog showError: messageStream contents. false ]
		 ifTrue: [
         checkVariableDefinition check ifFalse: [
        		checkVariableDefinition violationMessageOn: messageStream.
            dialog showError: messageStream contents. false ]
			 ifTrue: [ true ]].
	] whileFalse.
    
    refactoring checkPreconditions.
    self applyChanges.
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
