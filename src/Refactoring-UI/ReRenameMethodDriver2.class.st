"
I'm a model for user interaction for the rename method refactoring.
"
Class {
	#name : 'ReRenameMethodDriver2',
	#superclass : 'ReInteractionDriver',
	#instVars : [
		'class',
		'originalMessage',
		'newMessage',
		'shouldEscape',
		'methodNameEditorPresenterClass',
		'oldName',
		'newName',
		'conflictingImplementors'
	],
	#category : 'Refactoring-UI-Drivers',
	#package : 'Refactoring-UI',
	#tag : 'Drivers'
}

{ #category : 'accessing' }
ReRenameMethodDriver2 class >> driverName [

	^ 'Rename method'
]

{ #category : 'initialization' }
ReRenameMethodDriver2 >> applicabilityPreconditionsAreSatisfied [

	^ self checkApplicabilityPreconditions isNil
]

{ #category : 'execution' }
ReRenameMethodDriver2 >> breakingChoices [

	| items |
	items := OrderedCollection new.
	items add: (ReRenameMethodChoice new driver: self).
	items add: (ReRenameAndBrowseMethodOverridesChoice new driver: self).
	items add: (ReBrowseMethodOverridesChoice new driver: self).
	^ items
]

{ #category : 'actions' }
ReRenameMethodDriver2 >> browseOverrides [

	| overrides |
	overrides := conflictingImplementors violators.
	(Smalltalk tools toolNamed: #messageList)
		browse: (overrides collect: [ :ref | ref realClass lookupSelector: newMessage selector ])
		asImplementorsOf: newMessage selector
		
]

{ #category : 'private - testing' }
ReRenameMethodDriver2 >> canAddArgs [
	^ false
]

{ #category : 'private - testing' }
ReRenameMethodDriver2 >> canEditName [
	^ true
]

{ #category : 'private - testing' }
ReRenameMethodDriver2 >> canRemoveArgs [
	^ false
]

{ #category : 'private - testing' }
ReRenameMethodDriver2 >> canRenameArgs [
	^ true
]

{ #category : 'execution' }
ReRenameMethodDriver2 >> changes [
	"Remember should not call generateChanges"
	^ refactoring renameChanges


]

{ #category : 'actions' }
ReRenameMethodDriver2 >> checkApplicabilityPreconditions [

	| failedCondition |
	
	refactoring newName: newName.
	refactoring className: class .
	
	failedCondition := refactoring checkApplicabilityPreconditions
		detect: [ :condition | condition check not ]
		ifNone: [ nil ].
		
	failedCondition ifNotNil: [ 
		^ failedCondition errorString ].
	
	^ nil
]

{ #category : 'private - testing' }
ReRenameMethodDriver2 >> checkBehaviorPreconditions [

	| stream |
	
	conflictingImplementors := self refactoring superclassesDoNotDefineNewSelectorCondition .
	conflictingImplementors check ifTrue: [ ^ nil ].
	
	stream := String new writeStream .
	conflictingImplementors violationMessageOn: stream .
	^ stream contents 
]

{ #category : 'execution' }
ReRenameMethodDriver2 >> configureMessage [

	refactoring newSelector: newMessage selector.
	refactoring permutation: newMessage permutation.
	refactoring renameMap: newMessage renameMap.
]

{ #category : 'actions' }
ReRenameMethodDriver2 >> createAndSetNewRefactoring [

	super configureRefactoring .
	refactoring newName: newName
]

{ #category : 'execution' }
ReRenameMethodDriver2 >> gatherUserInput [

	| failedConditions |
	failedConditions := OrderedCollection empty.
	[
		newMessage := self requestNewMessage.
		shouldEscape ifTrue: [ ^ false ].
		self configureMessage.

		refactoring isMethodDefinitionUnchanged ifTrue: [ ^ false ].
		shouldEscape ifTrue: [ ^ false ].

		failedConditions := refactoring failedApplicabilityPreconditions.
		failedConditions do: [ :cond |
				self informDialog
					message: cond errorString;
					title: 'Invalid method name';
					openModal ] ] doWhileFalse: [ failedConditions isEmpty ].

	^ true
]

{ #category : 'initialization' }
ReRenameMethodDriver2 >> hasFailedBehaviorPreservingPreconditions [
		
	^ refactoring failedBreakingChangePreconditions notEmpty 
]

{ #category : 'initialization' }
ReRenameMethodDriver2 >> initialize [ 
	
	super initialize.
	shouldEscape := false.
]

{ #category : 'configuration' }
ReRenameMethodDriver2 >> instantiateRefactoring [
	"here we do not fully configure the refactoring because we are missing information such as the signature.
	Since we want to let the refactoring validate input we need to create it upfront then configure later."

	^ ReRenameMethodRefactoring new 
		renameMethod: originalMessage selector 
		in: class
]

{ #category : 'for mocks' }
ReRenameMethodDriver2 >> methodNameEditorPresenterClass [

	^ methodNameEditorPresenterClass ifNil: [ methodNameEditorPresenterClass := StMethodNameEditorPresenter]
]

{ #category : 'for mocks' }
ReRenameMethodDriver2 >> methodNameEditorPresenterClass: aClass [

	methodNameEditorPresenterClass := aClass
]

{ #category : 'initialization' }
ReRenameMethodDriver2 >> model: aModel renameMethodSignature: aMessage in: aClass [ 

	model := aModel.
	originalMessage := aMessage.
	"while we could think that the class is not needed, it is because a rename should not override existing methods."
	class := aClass
]

{ #category : 'actions' }
ReRenameMethodDriver2 >> newName: aSymbol [

	newName := aSymbol
]

{ #category : 'private - factory' }
ReRenameMethodDriver2 >> newRequestMethodDialogOn: methodName [
	
	^ (self methodNameEditorPresenterClass newApplication: self application model: methodName)
		invalidArgNames: (RBCondition
			invalidArgumentNamesForSelector:
			originalMessage selector
			in: model);
		canRenameArgs: self canRenameArgs;
		canRemoveArgs: self canRemoveArgs;
		canAddArgs: self canAddArgs;
		canEditName: self canEditName
]

{ #category : 'accessing' }
ReRenameMethodDriver2 >> oldName: aString [ 
	oldName := aString
]

{ #category : 'initialization' }
ReRenameMethodDriver2 >> originalMessage [ 
	^ originalMessage 
]

{ #category : 'actions' }
ReRenameMethodDriver2 >> previewPresenterClass [

	^ previewPresenterClass ifNil: [ previewPresenterClass := StRenameMethodPreviewPresenter ]
]

{ #category : 'actions' }
ReRenameMethodDriver2 >> renameMethod [

	self applyChanges 
]

{ #category : 'accessing' }
ReRenameMethodDriver2 >> requestDialogWith: methodName [
	"This is lazy loaded and tests expect lazy loading, because they set `requestDialog`
	and thus side-step methodNameEditorPresenter"

	requestDialog ifNil: [ requestDialog := self newRequestMethodDialogOn: methodName ].
	requestDialog openModal.
	^ requestDialog
]

{ #category : 'interaction' }
ReRenameMethodDriver2 >> requestNewMessage [

	| methodName parameters permutation |
	
	parameters := originalMessage selector implementors
		ifNotEmpty: [ :m | m first argumentNames ]
		ifEmpty: [ refactoring buildParametersForSelector: originalMessage selector ].

	methodName := RBMethodName
		selector: originalMessage selector
		arguments: parameters.

	permutation := (1 to: parameters size) asArray.
	methodName permutation: permutation.

	^ methodName
]

{ #category : 'actions' }
ReRenameMethodDriver2 >> runRefactoring [

	| presenter | 
	
	super configureRefactoring .
	
	newMessage := self requestNewMessage.
	
	presenter := (self previewPresenterClass newApplication: self application)
		scopes: scopes;
		driver: self;
		name: refactoring oldSelector asSymbol;
		arguments: newMessage arguments;
		openModal
	
	
]

{ #category : 'initialization' }
ReRenameMethodDriver2 >> scopes: refactoringScope model: aModel renameMethodSignature: aMessage in: aClass [

	scopes := refactoringScope.
	self model: aModel renameMethodSignature: aMessage in: aClass 
]

{ #category : 'actions' }
ReRenameMethodDriver2 >> setNewMessage: aSymbol [

	refactoring newSelector: aSymbol .
	newMessage selector: aSymbol .
	self configureMessage
]
