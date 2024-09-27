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

{ #category : 'initialization' }
ReExtractToTemporaryDriver >> initialize [ 
	
	super initialize.
	shouldEscape := false.
]

{ #category : 'execution' }
ReExtractToTemporaryDriver >> requestSelectorBasedOn: aSelector [

	selector := self defaultRequestDialog 
						title: 'Please provide a selector';
						label: 'The selector should be defined.';
						validateAnswer: [ :string :presenter |
							self validateSelector: string onPresenter: presenter ];
						text: aSelector printString;
						openModal.
	^ selector
						
]

{ #category : 'execution' }
ReExtractToTemporaryDriver >> requestVariableNameBasedOn: aName [

	newVariableName := self defaultRequestDialog
								title: 'Please provide a new variable name';
								label: 'The new name should be valid and not used.';
        						validateAnswer: [ :string :presenter | 
            						self validateName: string onPresenter: presenter ];
       						text: aName;
        						openModal.
	^ newVariableName
]

{ #category : 'execution' }
ReExtractToTemporaryDriver >> runRefactoring [

	self configureRefactoring.
	selector := self requestSelectorBasedOn: refactoring selector.
	shouldEscape ifTrue: [ ^ self ].
	
	newVariableName := self requestVariableNameBasedOn: refactoring newVariableName.
	shouldEscape ifTrue: [ ^ self ].
	self applyChanges
]

{ #category : 'validation' }
ReExtractToTemporaryDriver >> validateName: aName onPresenter: presenter [

	| firstCondition secondCondition result |
	firstCondition := ReIsValidInstanceVariableName new name: aName.
	secondCondition := ReCheckVariableNameCondition
		 class: refactoring class
		 variableName: aName
		 parseTree: refactoring parseTree.
	result := (firstCondition check and: [ secondCondition check ]).
	
	result 
		ifFalse: [ presenter setErrorText: 'Variable not valid or already defined.'. shouldEscape := true ] 
		ifTrue: [ presenter setErrorText: '' ].
		
	^ result
]

{ #category : 'private' }
ReExtractToTemporaryDriver >> validateSelector: aSelector onPresenter: presenter [

	| condition result |
	condition := ReDefinesSelectorsCondition new 
		definesSelectors: (Array with: aSelector ) in: refactoring class.
	result := condition check.
	
	result
		ifFalse: [ presenter setErrorText: 
										'The selector ', aSelector printString, ' is not defined in the class.'. 
					shouldEscape := true ]
	  	ifTrue: [ presenter setErrorText: '' ].
	
	^ result 
]
