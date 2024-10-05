Class {
	#name : 'ReExtractToTemporaryDriver',
	#superclass : 'RBInteractionDriver',
	#instVars : [
		'sourceInterval',
		'newVariableName',
		'selector',
		'class',
		'shouldEscape'
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

{ #category : 'execution' }
ReExtractToTemporaryDriver >> requestSelector [

	^ self requestUserInput: 'Please provide a selector'
			 label: 'The selector should be defined.'  
			 validate: [ :string :presenter | self validateSelector: string onPresenter: presenter ]
]

{ #category : 'execution' }
ReExtractToTemporaryDriver >> requestUserInput: aTitle label: aLabel validate: aValidationBlock [

	| userInput |
	userInput := self defaultRequestDialog
												title: aTitle;
												label: aLabel;
												validateAnswer: aValidationBlock;
												openModal.
	^ userInput
]

{ #category : 'execution' }
ReExtractToTemporaryDriver >> requestVariableName [

	^ self requestUserInput: 'Please provide a variable name'
			 label: 'The variable name should be defined.'  
			 validate: [ :string :presenter | self validateName: string onPresenter: presenter ]
]

{ #category : 'execution' }
ReExtractToTemporaryDriver >> runRefactoring [

	self configureRefactoring.
	
	selector := self requestSelector.
	shouldEscape ifTrue: [ ^ self ].
	
	refactoring prepareForExecution.
	
	newVariableName := self requestVariableName.
	shouldEscape ifTrue: [ ^ self ].
	
	self applyChanges
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

	| firstCondition secondCondition result |
	firstCondition := ReIsValidInstanceVariableName new name: aName.
	secondCondition := ReCheckVariableNameCondition
		 class: class
		 variableName: aName
		 parseTree: refactoring parseTree.
	result := (firstCondition check and: [ secondCondition check ]).
	
	result 
		ifFalse: [
			firstCondition check ifFalse: [
				self setErrorTextFromCondition: firstCondition onPresenter: presenter ].
			secondCondition check ifFalse: [ 
				self setErrorTextFromCondition: secondCondition onPresenter: presenter ].
			shouldEscape := true ] 
		ifTrue: [ presenter setErrorText: '' ].
		
	^ result
]

{ #category : 'private' }
ReExtractToTemporaryDriver >> validateSelector: aSelector onPresenter: presenter [

	| condition violators |
	condition := ReDefinesSelectorsCondition new 
		definesSelectors: { selector } in: class.
	violators := condition violators.
	
 	violators isEmpty
		ifFalse: [ self setErrorTextFromCondition: condition onPresenter: presenter . ^ false ]
	  	ifTrue: [ presenter setErrorText: ''. ^ true ]. 
]
