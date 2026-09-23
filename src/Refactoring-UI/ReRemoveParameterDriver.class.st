"
I am a driver to remove a parameter from a method. I gather the new signature from the user, check the preconditions, warn about breaking changes and invoke RBRemoveParameterRefactoring.
"
Class {
	#name : 'ReRemoveParameterDriver',
	#superclass : 'ReInteractionDriver',
	#instVars : [
		'originalMessage',
		'newMessage',
		'class',
		'removerPresenterClass'
	],
	#category : 'Refactoring-UI-Drivers',
	#package : 'Refactoring-UI',
	#tag : 'Drivers'
}

{ #category : 'accessing' }
ReRemoveParameterDriver class >> driverName [

	^ 'Remove argument'
]

{ #category : 'breaking changes' }
ReRemoveParameterDriver >> b [

	
]

{ #category : 'breaking changes' }
ReRemoveParameterDriver >> breakingChoices [
	
	^ {
		ReRemoveParameterChoice new
			driver: self;
			yourself.
		ReBrowseMethodOverridesChoice new
			driver: self;
			description: 'Browse existing methods that will be overriden BUT don''t remove the parameter';
			yourself.
	}
]

{ #category : 'breaking changes' }
ReRemoveParameterDriver >> browseOverrides [
	| overrides |

	overrides := refactoring violations.
	
	(Smalltalk tools toolNamed: #messageList)
		browse: (overrides collect: [ :ref | ref realClass lookupSelector: newMessage selector ])
		asImplementorsOf: newMessage selector
]

{ #category : 'configuration' }
ReRemoveParameterDriver >> configureMessage [

	refactoring removeParameter: self removedArgumentName in: class selector: originalMessage selector.
	refactoring renameMap: newMessage renameMap
]

{ #category : 'breaking changes' }
ReRemoveParameterDriver >> defaultSelectDialog [

	^ super defaultSelectDialog
		  title: 'There are potential breaking changes!';
		  message: 'Watchout potential override detected!';
		  items: self breakingChoices;
		  display: [ :each | each description ];
		  displayIcon: [ :each | self iconNamed: each systemIconName ];
		  yourself
]

{ #category : 'user interface' }
ReRemoveParameterDriver >> gatherUserInput [
	| failedConditions |
	
	failedConditions := OrderedCollection empty.
	[
		newMessage := self requestNewMessage.
		newMessage ifNil: [ ^ false ].
		self configureMessage.
		self isMethodDefinitionUnchanged ifTrue: [ ^ false ].
		failedConditions := refactoring failedApplicabilityPreconditions.
		failedConditions do: [ :condition |
			self inform: condition errorString ] 
	] 
	doWhileFalse: [ 
		failedConditions isEmpty ].
	
	^ true
]

{ #category : 'refactoring' }
ReRemoveParameterDriver >> instantiateRefactoring [
	"Here we do not fully configure the refactoring: the argument to remove is missing.
	Since we want to let the refactoring validate input, we create it upfront and configure it later."

	^ RBRemoveParameterRefactoring new removeParameterFrom: originalMessage selector in: class
]

{ #category : 'testing' }
ReRemoveParameterDriver >> isMethodDefinitionUnchanged [

	^ newMessage arguments = originalMessage argumentNames and: [ newMessage renameMap isEmpty ]
]

{ #category : 'initialization' }
ReRemoveParameterDriver >> model: aModel removeParameterFromMethodSignature: aMessage in: aClass [

	model := aModel.
	originalMessage := aMessage.
	class := aClass
]

{ #category : 'user interface' }
ReRemoveParameterDriver >> newRequestRemovalDialogOn: methodName [

	^ (self removerPresenterClass newApplication: self application model: methodName)
		  invalidArgNames: (RBCondition invalidArgumentNamesForSelector: originalMessage selector in: model);
		  yourself
]

{ #category : 'actions' }
ReRemoveParameterDriver >> removeParameter [

	self applyChanges
]

{ #category : 'accessing' }
ReRemoveParameterDriver >> removedArgumentName [
	"The argument that is not in the resulting signature, neither as itself nor renamed."
	| renamedNames |
	
	renamedNames := newMessage renameMap collect: [ :each | each name ].
	^ originalMessage argumentNames detect: [ :each |
		  (newMessage arguments includes: each) not 
		  	and: [ (renamedNames includes: each) not ] ]
]

{ #category : 'accessing' }
ReRemoveParameterDriver >> removerPresenterClass [

	^ removerPresenterClass ifNil: [ removerPresenterClass := StMethodArgumentsRemoverPresenter ]
]

{ #category : 'accessing' }
ReRemoveParameterDriver >> removerPresenterClass: aPresenterClass [

	removerPresenterClass := aPresenterClass
]

{ #category : 'user interface' }
ReRemoveParameterDriver >> requestDialogWith: methodName [
	"I am lazily loading the dialog. It also allows tests to inject a mock."

	requestDialog ifNil: [ requestDialog := self newRequestRemovalDialogOn: methodName ].
	requestDialog openModal.
	^ requestDialog
]

{ #category : 'user interface' }
ReRemoveParameterDriver >> requestNewMessage [
	| parameters methodName dialog |
	
	parameters := originalMessage argumentNames.
	methodName := RBMethodName selector: originalMessage selector arguments: parameters.
	dialog := self requestDialogWith: methodName.
	dialog cancelled ifTrue: [
		^ nil ].
	
	^ dialog methodName
]

{ #category : 'initialization' }
ReRemoveParameterDriver >> scopes: refactoringScope model: aModel removeParameterFromMethodSignature: aMessage in: aClass [

	scopes := refactoringScope.
	self model: aModel removeParameterFromMethodSignature: aMessage in: aClass
]
