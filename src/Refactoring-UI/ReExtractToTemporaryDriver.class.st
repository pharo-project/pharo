Class {
	#name : 'ReExtractToTemporaryDriver',
	#superclass : 'RBInteractionDriver',
	#instVars : [
		'sourceInterval',
		'newVariableName',
		'selector',
		'class',
		'shouldEscape',
		'variableNameEditorPresenterClass',
		'newMessage'
	],
	#category : 'Refactoring-UI-Drivers',
	#package : 'Refactoring-UI',
	#tag : 'Drivers'
}

{ #category : 'testing' }
ReExtractToTemporaryDriver >> canEditName [
	^ true
]

{ #category : 'testing' }
ReExtractToTemporaryDriver >> canRenameArgs [
	^ false
]

{ #category : 'execution' }
ReExtractToTemporaryDriver >> changes [
	
	^ refactoring privateTransform; changes

]

{ #category : 'execution' }
ReExtractToTemporaryDriver >> configureMessage [
	refactoring newSelector: newMessage selector.
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
ReExtractToTemporaryDriver >> model: aModel [
	model := aModel
]

{ #category : 'execution' }
ReExtractToTemporaryDriver >> requestVariableName [

	newVariableName := self defaultRequestDialog
										title: 'Please provide a variable name';
										label: 'The variable name should be defined.';
										validateAnswer:[ :string :presenter |
											self validateName: string onPresenter: presenter ];
										openModal.
	^ newVariableName 
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
	scopes := aCollection
]

{ #category : 'initialization' }
ReExtractToTemporaryDriver >> setErrorTextFromCondition: aCondition onPresenter: aPresenter [

	| stream |
	stream := WriteStream on: String new.
	aCondition violationMessageOn: stream.
	aPresenter setErrorText: stream contents
]

{ #category : 'validation' }
ReExtractToTemporaryDriver >> validateName: aName onPresenter: presenter [
 
	| firstCondition secondCondition result errorMessages stream |
   	firstCondition := refactoring preconditionValidInstVarName.
	secondCondition := refactoring preconditionCheckVariableName.
	stream := WriteStream on: String new.

	errorMessages := OrderedCollection new.
	
	result := (firstCondition check and: [ secondCondition check ]).
    
   result 
       ifFalse: [ 
           firstCondition check ifFalse: [ 
               self setErrorTextFromCondition: firstCondition onPresenter: presenter.
               errorMessages add: (firstCondition violationMessageOn: stream).
           ].
           secondCondition check ifFalse: [ 
               self setErrorTextFromCondition: secondCondition onPresenter: presenter.
               errorMessages add: (secondCondition violationMessageOn: stream ).
           ].
           shouldEscape := true.
       ]
       ifTrue: [ presenter setErrorText: '' ].

	errorMessages isEmpty
		ifFalse: [ 
           presenter setErrorText: (errorMessages join: ', ').
       ]. 
   
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
