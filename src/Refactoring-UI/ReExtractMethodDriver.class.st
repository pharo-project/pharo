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

{ #category : 'accessing' }
ReExtractMethodDriver class >> driverName [
	"I'm not sure we should keep the `no occurences`? In the future this driver should probably manage all options"

	^ 'Extract method (no occurrences)'
]

{ #category : 'execution' }
ReExtractMethodDriver >> breakingChoices [

	| items |
	items := OrderedCollection new.
	
	(notOverride check and: sameExitPoint check) ifFalse: [
			items add: (ReExtractMethodChoice new
					 driver: self;
					 yourself) ].

	notOverride check ifFalse: [ "These choices are exclusive to notOverride"
			items add: (ReExtractAndBrowseMethodOverridesChoice new
					 driver: self;
					 yourself).
			items add: (ReBrowseMethodOverridesChoice new
					 driver: self;
					 description: 'Browse existing methods that will be overriden BUT don''t extract';
					 yourself) ].

	^ items
]

{ #category : 'actions' }
ReExtractMethodDriver >> browseOverrides [

	| overrides |
	overrides := notOverride violators collect: [ :cls | cls realClass methodNamed: newMessage methodName asSymbol ].
	
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

{ #category : 'configuration' }
ReExtractMethodDriver >> defaultSelectDialog [

	^ super defaultSelectDialog
		title: 'There are potential breaking changes!';
		label: self labelBasedOnBreakingChanges;
		items: self breakingChoices;
		display: [ :each | each description ];
		displayIcon: [ :each | self iconNamed: each systemIconName ];
		yourself
]

{ #category : 'instance creation' }
ReExtractMethodDriver >> extract: aString from: compiledMethod in: aClass [

	class := aClass.
	sourceMethod := compiledMethod.
	body := aString
]

{ #category : 'actions' }
ReExtractMethodDriver >> extractMethod [
	
	^ self applyChanges
]

{ #category : 'execution' }
ReExtractMethodDriver >> gatherUserInput [

	newMessage := self requestNewMessage.
	newMessage ifNil: [ "The user has cancelled the dialog" ^ false ].
	refactoring newSelector: newMessage selector.
	^ true
]

{ #category : 'private exe' }
ReExtractMethodDriver >> hasFailedBehaviorPreservingPreconditions [

	notOverride := refactoring preconditionNoOverrides.
	sameExitPoint := refactoring preconditionHasSameExitPoint.
	^ refactoring failedBreakingChangePreconditions isNotEmpty 
]

{ #category : 'configuration' }
ReExtractMethodDriver >> instantiateRefactoring [
    "here we do not fully configure the refactoring because we are missing information such as the signature.
    Since we want to let the refactoring validate input we need to create it upfront then configure later."
    
	^ ReCompositeExtractMethodRefactoring new extract: body from: sourceMethod selector in: class name
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

	^ methodNameEditorPresenterClass ifNil: [ methodNameEditorPresenterClass := StMethodNameEditorPresenter ]
]

{ #category : 'accessing' }
ReExtractMethodDriver >> methodNameEditorPresenterClass: aClass [

	methodNameEditorPresenterClass := aClass
]

{ #category : 'accessing' }
ReExtractMethodDriver >> requestDialogWith: methodName [
	"This is lazy loaded and tests expect lazy loading, because they set `requestDialog`
	and thus side-step methodNameEditorPresenter"

	^ requestDialog ifNil: [
		requestDialog := ((self methodNameEditorPresenterClass newApplication: self application model: methodName)
				invalidArgNames: (RBCondition
						invalidArgumentNamesForSelector:
						sourceMethod selector
						in: model);
				canRenameArgs: self canRenameArgs;
				canRemoveArgs: self canRemoveArgs;
				canAddArgs: self canAddArgs; 
				canEditName: self canEditName; 
				openModal)
			presenter ]
]

{ #category : 'interaction' }
ReExtractMethodDriver >> requestNewMessage [

	| methodName dialog |
	dialog := nil.
	methodName := refactoring newMethodName.
	"Retry until the methodName is valid or the user cancels the dialog"
	[ dialog isNil or: [ dialog methodName isValid not ] ] whileTrue: [
			requestDialog := nil.
			dialog := self requestDialogWith: methodName.
			dialog cancelled ifTrue: [ ^ nil ] ].
	^ dialog methodName
]
