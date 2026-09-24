"
I am a driver to add a parameter to a method. I gather the new signature from the user, check the preconditions, warn about breaking changes and invoke RBAddParameterRefactoring.
"
Class {
	#name : 'ReAddParameterDriver',
	#superclass : 'ReInteractionDriver',
	#instVars : [
		'originalMessage',
		'newMessage',
		'class',
		'methodNameEditorPresenterClass'
	],
	#category : 'Refactoring-UI-Drivers',
	#package : 'Refactoring-UI',
	#tag : 'Drivers'
}

{ #category : 'accessing' }
ReAddParameterDriver class >> driverName [

	^ 'Add argument'
]

{ #category : 'actions' }
ReAddParameterDriver >> addParameter [

	self applyChanges
]

{ #category : 'breaking changes' }
ReAddParameterDriver >> breakingChoices [

	^ {
		ReAddParameterChoice new 
			driver: self;
			yourself.
		ReBrowseMethodOverridesChoice new 
			driver: self;
			description: 'Browse existing methods that will be overriden BUT don''t add the parameter';
			yourself.
	}
]

{ #category : 'breaking changes' }
ReAddParameterDriver >> browseOverrides [
   | overrides |
	   
	overrides := refactoring violations.
	(Smalltalk tools toolNamed: #messageList)
		browse: (overrides collect: [ :ref | ref realClass lookupSelector: newMessage selector ])
	asImplementorsOf: newMessage selector
]

{ #category : 'accessing' }
ReAddParameterDriver >> canAddArgs [

	^ true
]

{ #category : 'accessing' }
ReAddParameterDriver >> canEditName [

	^ false
]

{ #category : 'accessing' }
ReAddParameterDriver >> canRemoveArgs [

	^ false
]

{ #category : 'accessing' }
ReAddParameterDriver >> canRenameArgs [

	^ true
]

{ #category : 'configuration' }
ReAddParameterDriver >> configureMessage [

	refactoring
		newSelector: newMessage selector;
		permutation: newMessage permutation;
		newArgs: newMessage newArgs;
		renameMap: newMessage renameMap
]

{ #category : 'breaking changes' }
ReAddParameterDriver >> defaultSelectDialog [

	^ super defaultSelectDialog
		title: 'There are potential breaking changes!';
		message: 'Watchout potential override detected!';
		items: self breakingChoices;
		display: [ :each | each description ];
		displayIcon: [ :each | self iconNamed: each systemIconName ];
		yourself
]

{ #category : 'user interface' }
ReAddParameterDriver >> gatherUserInput [
	| failedConditions |
	
	failedConditions := OrderedCollection new.
	[
		newMessage := self requestNewMessage.
		newMessage ifNil: [ ^ false ].
		self configureMessage.
		self isMethodDefinitionUnchanged ifTrue: [ ^ false ].
		failedConditions := refactoring failedApplicabilityPreconditions.
		failedConditions do: [ :condition |
			self inform: ('{1}: Invalid method signature' format: { condition errorString }) ] 
	]
	doWhileFalse: [ 
		failedConditions isEmpty ].
	
	^ true
]

{ #category : 'refactoring' }
ReAddParameterDriver >> instantiateRefactoring [
	"Here we do not fully configure the refactoring: the new signature is missing.
	Since we want to let the refactoring validate input, we create it upfront and configure it later."

	^ RBAddParameterRefactoring new 
		renameMethod: originalMessage selector in: class;
		yourself
]

{ #category : 'testing' }
ReAddParameterDriver >> isMethodDefinitionUnchanged [

	^ newMessage selector = originalMessage selector 
		and: [ newMessage newArgs isEmpty 
		and: [ newMessage renameMap isEmpty 
		and: [ self isPermutationIdentity ] ] ]
]

{ #category : 'testing' }
ReAddParameterDriver >> isPermutationIdentity [
	"Answer true if the arguments keep their original order (no reordering)."

	^ newMessage permutation asArray = (1 to: originalMessage argumentNames size) asArray
]

{ #category : 'accessing' }
ReAddParameterDriver >> methodNameEditorPresenterClass [

	^ methodNameEditorPresenterClass ifNil: [ methodNameEditorPresenterClass := StMethodNameEditorPresenter ]
]

{ #category : 'accessing' }
ReAddParameterDriver >> methodNameEditorPresenterClass: aPresenterClass [

	methodNameEditorPresenterClass := aPresenterClass
]

{ #category : 'initialization' }
ReAddParameterDriver >> model: aModel addParameterToMethodSignature: aMessage in: aClass [

	model := aModel.
	originalMessage := aMessage.
	class := aClass
]

{ #category : 'user interface' }
ReAddParameterDriver >> newRequestMethodDialogOn: methodName [

	^ (self methodNameEditorPresenterClass newApplication: self application model: methodName)
		  invalidArgNames: (RBCondition invalidArgumentNamesForSelector: originalMessage selector in: model);
		  canRenameArgs: self canRenameArgs;
		  canRemoveArgs: self canRemoveArgs;
		  canAddArgs: self canAddArgs;
		  canEditName: self canEditName
]

{ #category : 'user interface' }
ReAddParameterDriver >> requestDialogWith: methodName [
	"I am lazily loading the dialog. It also allows tests to inject a mock."

	requestDialog ifNil: [ requestDialog := self newRequestMethodDialogOn: methodName ].
	requestDialog openModal.
	^ requestDialog
]

{ #category : 'user interface' }
ReAddParameterDriver >> requestNewMessage [
	| parameters methodName dialog |
	
	parameters := originalMessage argumentNames.
	methodName := RBMethodName selector: originalMessage selector arguments: parameters.
	dialog := self requestDialogWith: methodName.
	dialog cancelled 
		ifTrue: [ ^ nil ].

	^ dialog methodName
]

{ #category : 'initialization' }
ReAddParameterDriver >> scopes: refactoringScope model: aModel addParameterToMethodSignature: aMessage in: aClass [

	scopes := refactoringScope.
	self model: aModel addParameterToMethodSignature: aMessage in: aClass
]
