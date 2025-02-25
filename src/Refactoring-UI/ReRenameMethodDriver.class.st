"
I'm a model for user interaction for the rename method refactoring.
"
Class {
	#name : 'ReRenameMethodDriver',
	#superclass : 'ReInteractionDriver',
	#instVars : [
		'class',
		'originalMessage',
		'newMessage',
		'shouldEscape',
		'methodNameEditorPresenterClass'
	],
	#category : 'Refactoring-UI-Drivers',
	#package : 'Refactoring-UI',
	#tag : 'Drivers'
}

{ #category : 'execution' }
ReRenameMethodDriver >> breakingChoices [

	| items |
	items := OrderedCollection new.
	items add: (ReRenameMethodChoice new driver: self).
	items add: (ReRenameAndBrowseMethodOverridesChoice new driver: self).
	items add: (ReBrowseMethodOverridesChoice new driver: self).
	^ items
]

{ #category : 'actions' }
ReRenameMethodDriver >> browseOverrides [

	| overrides |
	overrides := refactoring violations.
	StMessageBrowser
		browse: (overrides collect: [ :ref | ref realClass methodNamed: newMessage selector ])
		asImplementorsOf: newMessage selector
]

{ #category : 'private testing' }
ReRenameMethodDriver >> canAddArgs [
	^ false
]

{ #category : 'private testing' }
ReRenameMethodDriver >> canEditName [
	^ true
]

{ #category : 'private testing' }
ReRenameMethodDriver >> canRemoveArgs [
	^ false
]

{ #category : 'private testing' }
ReRenameMethodDriver >> canRenameArgs [
	^ true
]

{ #category : 'execution' }
ReRenameMethodDriver >> changes [
	"Remember should not call generateChanges"
	^ refactoring renameChanges


]

{ #category : 'execution' }
ReRenameMethodDriver >> configureMessage [

	refactoring newSelector: newMessage selector.
	refactoring permutation: newMessage permutation.
	refactoring renameMap: newMessage renameMap.
]

{ #category : 'execution' }
ReRenameMethodDriver >> configureRefactoring [

	"here we do not fully configure the refactoring because we are missing information such as the signature.
	Since we want to let the refactoring validate input we need to create it upfront then configure later."
	
	refactoring := ReRenameMethodRefactoring new renameMethod: originalMessage selector in: class 
]

{ #category : 'configuration' }
ReRenameMethodDriver >> defaultSelectDialog [ 

	^ SpSelectDialog new
		    title: 'There are potential breaking changes!';
		          label: 'Watchout potential override detected!';
		          items: self breakingChoices;
		          display: [ :each | each description ];
		          displayIcon: [ :each | self iconNamed: each systemIconName ];
		          openModal
]

{ #category : 'execution' }
ReRenameMethodDriver >> handleBreakingChanges [

	| select |
	select := self selectDialog.
	select ifNotNil: [ select action ]
]

{ #category : 'initialization' }
ReRenameMethodDriver >> initialize [ 
	
	super initialize.
	shouldEscape := false.
]

{ #category : 'for mocks' }
ReRenameMethodDriver >> methodNameEditorPresenterClass [

	^ methodNameEditorPresenterClass ifNil: [ methodNameEditorPresenterClass := StMethodNameEditorPresenter]
]

{ #category : 'for mocks' }
ReRenameMethodDriver >> methodNameEditorPresenterClass: aClass [

	methodNameEditorPresenterClass := aClass
]

{ #category : 'initialization' }
ReRenameMethodDriver >> model: aModel renameMethodSignature: aMessage in: aClass [ 

	model := aModel.
	originalMessage := aMessage.
	"while we could think that the class is not needed, it is because a rename should not override existing methods."
	class := aClass
]

{ #category : 'actions' }
ReRenameMethodDriver >> renameMethod [

	self applyChanges 
]

{ #category : 'accessing' }
ReRenameMethodDriver >> requestDialogWith: methodName [
	"This is lazy loaded and tests expect lazy loading, because they set `requestDialog`
	and thus side-step methodNameEditorPresenter"

	^ requestDialog := (self methodNameEditorPresenterClass
		                    openOn: methodName
		                    withInvalidArgs: (RBCondition
				                     invalidArgumentNamesForSelector:
				                     originalMessage selector
				                     in: model)
		                    canRenameArgs: self canRenameArgs
		                    canRemoveArgs: self canRemoveArgs
		                    canAddArgs: self canAddArgs
		                    canEditName: self canEditName) presenter
]

{ #category : 'interaction' }
ReRenameMethodDriver >> requestNewMessage [

	| methodName dialog parameters |
	
	"when we rename from a method list or clicking on the method signature in the method body
	the parameters passed to the refactoring are ok. 
	Now when the user selected a call site the arguments make no sense.
	"
	parameters := originalMessage argumentNames.
	
	
	"We need a nice way to know whether a piece of code is an identifier
	parametersAreWrong := parameters anySatisfy: [ :each | (RBScanner on: each readStream) scanToken isIdentifier not ].
	parametersAreWrong .... Like that we could make the difference 
	bewteeen rename from call site and rename from method signature. "
							
	"For now always use the one of an implementor that is not necessarily 
	the one of method - this can happen that we rename a method clicking clickin on its signature
	and in this case we may have different parameters. "										
	parameters := originalMessage selector implementors 
							ifNotEmpty: [:m | m first  argumentNames ]
							ifEmpty: [ refactoring buildParamtersForSelector: originalMessage selector ].										
											
	methodName := RBMethodName
		              selector: originalMessage selector
		              arguments: parameters.
	dialog := self requestDialogWith: methodName.
	
	dialog ifNil: [ shouldEscape := true. ^ self ].

	^ dialog methodName
]

{ #category : 'execution' }
ReRenameMethodDriver >> runRefactoring [

	| failedConditions |
	self configureRefactoring.
	[ newMessage := self requestNewMessage.
	"note that it would be better to pass a method name to the refactoring instead of splitting the arguments this way"
	refactoring newSelector: newMessage selector; permutation: newMessage permutation.
	"If the name is the same then there is no need to perform the refactoring 
	and in such case we can just escape."
	refactoring newNameDoesNotRequireRefactoringPrecondition check
		ifTrue: [ ^ self ].
	shouldEscape ifTrue: [ ^ self ].
	failedConditions := refactoring failedApplicabilityPreconditions.
	failedConditions isEmpty ]
		whileFalse: [
			failedConditions do: [ :cond |
				"We need to mockify it"
				self informDialog
					label: cond errorString;
					title: 'Invalid method name';
					openModal ] ].
	
	refactoring areNamesTheSame ifTrue: [ ^ self ].

	"names are different so we should check because this is not a permutation" 
	refactoring failedBreakingChangePreconditions 
		ifEmpty: [ 	
			self configureMessage.
			self applyChanges ]
		ifNotEmpty: [ self handleBreakingChanges ]

]

{ #category : 'initialization' }
ReRenameMethodDriver >> scopes: refactoringScope model: aModel renameMethodSignature: aMessage in: aClass [

	scopes := refactoringScope.
	self model: aModel renameMethodSignature: aMessage in: aClass 
]
