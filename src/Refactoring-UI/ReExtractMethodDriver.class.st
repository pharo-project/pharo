Class {
	#name : 'ReExtractMethodDriver',
	#superclass : 'ReInteractionDriver',
	#instVars : [
		'class',
		'newMessage',
		'methodNameEditorPresenterClass',
		'body',
		'sourceMethod',
		'notOverride',
		'sameExitPoint'
	],
	#category : 'Refactoring-UI-Drivers',
	#package : 'Refactoring-UI',
	#tag : 'Drivers'
}

{ #category : 'displaying' }
ReExtractMethodDriver class >> refactoringClass [

	^ ReCompositeExtractMethodRefactoring
]

{ #category : 'execution' }
ReExtractMethodDriver >> breakingChoices [

	| items |
	items := OrderedCollection new.
	
	(notOverride check and: sameExitPoint check) ifFalse: [
			items add: (ReRenameMethodChoice new
					 driver: self;
					 description: 'Extract method';
					 yourself) ].

	notOverride check ifFalse: [ "These choices are exclusive to notOverride"
			items add: (ReRenameAndBrowseMethodOverridesChoice new
					 driver: self;
					 description: 'Extract method and browse existing methods that will then be overriden';
					 yourself).
			items add: (ReBrowseMethodOverridesChoice new
					 driver: self;
					 description: 'Browse existing methods that will be overriden BUT don''t extract';
					 yourself) ].

	^ items
]

{ #category : 'actions' }
ReExtractMethodDriver >> browseOverrides [

	| conditions overrides |
	conditions := refactoring failedBreakingChangePreconditions.
	overrides := conditions flatCollect: [ :cond |
		             cond violators collect: [ :cls | cls realClass methodNamed: newMessage methodName asSymbol ] ].
	overrides do: [ :method | method browse ]
]

{ #category : 'testing' }
ReExtractMethodDriver >> canAddArgs [
	^ false
]

{ #category : 'testing' }
ReExtractMethodDriver >> canEditName [
	^ true
]

{ #category : 'testing' }
ReExtractMethodDriver >> canRemoveArgs [
	^ false
]

{ #category : 'testing' }
ReExtractMethodDriver >> canRenameArgs [
	"for now false"
	^ false
]

{ #category : 'execution' }
ReExtractMethodDriver >> configureMessage [

	refactoring newSelector: newMessage selector.
	"refactoring permutation: newMessage permutation.
	refactoring renameMap: newMessage renameMap."
]

{ #category : 'resources' }
ReExtractMethodDriver >> configureRefactoring [

	"here we do not fully configure the refactoring because we are missing information such as the signature.
	Since we want to let the refactoring validate input we need to create it upfront then configure later."
	
	refactoring := self class refactoringClass new
		               extract: body
		               from: sourceMethod selector
		               in: class name.
	refactoring prepareForExecution 
]

{ #category : 'configuration' }
ReExtractMethodDriver >> defaultSelectDialog [

	^ self class defaultSelectDialog
		  title: 'There are potential breaking changes!';
		  label: self labelBasedOnBreakingChanges;
		  items: self breakingChoices;
		  display: [ :each | each description ];
		  displayIcon: [ :each | self iconNamed: each systemIconName ];
		  openModal
]

{ #category : 'instance creation' }
ReExtractMethodDriver >> extract: aString from: compiledMethod in: aClass [

	class := aClass.
	sourceMethod := compiledMethod.
	body := aString
]

{ #category : 'execution' }
ReExtractMethodDriver >> handleBreakingChanges [

	| select |
	select := self selectDialog.
	select ifNotNil: [ select action ]
]

{ #category : 'configuration' }
ReExtractMethodDriver >> labelBasedOnBreakingChanges [

	^ String streamContents: [ :stream |
			  refactoring failedBreakingChangePreconditions do: [ :cond |
					  cond violationMessageOn: stream.
					  stream cr ].
			  stream cr.
			  stream nextPutAll: 'Select a strategy' ]
]

{ #category : 'accessing' }
ReExtractMethodDriver >> methodNameEditorPresenterClass [

	^ methodNameEditorPresenterClass ifNil: [ methodNameEditorPresenterClass := StMethodNameEditorPresenter]
]

{ #category : 'accessing' }
ReExtractMethodDriver >> methodNameEditorPresenterClass: aClass [

	methodNameEditorPresenterClass := aClass
]

{ #category : 'actions' }
ReExtractMethodDriver >> renameMethod [
	"Eventhough we are not renaming methods, but extracting them, we implement this action to reuse both
	ReRenameAndBrowseMethodOverridesChoice and ReRenameMethodChoice"

	^ self applyChanges
]

{ #category : 'accessing' }
ReExtractMethodDriver >> requestDialogWith: methodName [
	"This is lazy loaded and tests expect lazy loading, because they set `requestDialog`
	and thus side-step methodNameEditorPresenter"

	^ requestDialog ifNil: [
		  requestDialog := (self methodNameEditorPresenterClass
			                   openOn: methodName
			                   withInvalidArgs: (RBCondition
					                    invalidArgumentNamesForSelector:
					                    sourceMethod selector
					                    in: model)
			                   canRenameArgs: self canRenameArgs
			                   canRemoveArgs: self canRemoveArgs
			                   canAddArgs: self canAddArgs
			                   canEditName: self canEditName) presenter ]
]

{ #category : 'interaction' }
ReExtractMethodDriver >> requestNewMessage [

	| methodName dialog |
	dialog := nil.
	methodName := RBMethodName
		              selector: (refactoring selectorStartingFrom: 'f' argumentsSize: refactoring arguments size)
		              arguments: refactoring arguments.
	"Retry until the methodName is valid or the user cancels the dialog"
	[ dialog isNil or: [ dialog methodName isValid not ] ] whileTrue: [
			requestDialog := nil.
			dialog := self requestDialogWith: methodName.
			dialog cancelled ifTrue: [ ^ nil ] ].
	^ dialog methodName
]

{ #category : 'execution' }
ReExtractMethodDriver >> runRefactoring [

	self configureRefactoring.
	
	refactoring failedApplicabilityPreconditions 
		ifNotEmpty: [ ^ self inform: self failedPreconditionsErrorString ].
	newMessage := self requestNewMessage.
	newMessage ifNil: [ "The user has cancelled the dialog" ^ self ].
	refactoring newSelector: newMessage selector.
	
	self setBreakingChangesPreconditions. 
	refactoring failedBreakingChangePreconditions
		ifEmpty: [ self applyChanges ]
		ifNotEmpty: [ self handleBreakingChanges ]
]

{ #category : 'configuration' }
ReExtractMethodDriver >> setBreakingChangesPreconditions [

	notOverride := refactoring preconditionNoOverrides.
	sameExitPoint := refactoring preconditionHasSameExitPoint
]
