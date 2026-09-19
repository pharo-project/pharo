"
I am a dialog to select an argument of a method and remove it from the signature. I also allow renaming the remaining arguments. I am used by ReRemoveParameterDriver.
"
Class {
	#name : 'StMethodArgumentsRemoverPresenter',
	#superclass : 'SpPresenter',
	#instVars : [
		'selectorInput',
		'argumentsList',
		'previewResult',
		'methodName',
		'args',
		'invalidArgNames',
		'requestor'
	],
	#category : 'Refactoring-UI',
	#package : 'Refactoring-UI'
}

{ #category : 'accessing' }
StMethodArgumentsRemoverPresenter >> argumentsList [

	^ argumentsList
]

{ #category : 'actions' }
StMethodArgumentsRemoverPresenter >> argumentsListActions [

	^ SpActionGroup new
		  addActionWith: [ :anItem |
				  anItem
					  name: 'Rename';
					  actionEnabled: [ argumentsList selectedItem canBeRenamed ];
					  iconName: #edit;
					  action: [ self renameArgument: argumentsList selectedItem ] ];
		  yourself
]

{ #category : 'testing' }
StMethodArgumentsRemoverPresenter >> canRemoveArgumentAt: anIndex [
	"Answer whether the argument at anIndex can be removed. The first argument
	carries the method name, so it can only be removed when it is the sole
	argument (the method then becomes unary)."

	^ anIndex > 1 or: [ methodName arguments size <= 1 ]
]

{ #category : 'TOREMOVE' }
StMethodArgumentsRemoverPresenter >> cancelled [
	
	^ self withWindowDo: #cancelled
]

{ #category : 'initialization' }
StMethodArgumentsRemoverPresenter >> connectPresenters [

	selectorInput whenTextChangedDo: [ :text | self updateLabel ].
	argumentsList whenSelectionChangedDo: [ :selection |
		selection selectedItem ifNotNil: [ selectorInput text: (self defaultSelectorAfterRemoving: selection selectedItem name) ] ].

	argumentsList
		items: (methodName arguments collect: [ :arg | RBArgumentName name: arg ]);
		actions: self argumentsListActions.

	argumentsList items ifNotEmpty: [ argumentsList selectIndex: self defaultRemovalIndex ]
]

{ #category : 'layout' }
StMethodArgumentsRemoverPresenter >> defaultLayout [

	^ SpBoxLayout newTopToBottom
		add: 'Argument to be removed' expand: false;
		add: argumentsList;
		add: 'Modified selector' expand: false;
		add: selectorInput expand: false;
		add: 'Preview' expand: false;
		add: previewResult expand: false;
		yourself
]

{ #category : 'accessing' }
StMethodArgumentsRemoverPresenter >> defaultRemovalIndex [
	"The first argument cannot be removed for keyword methods, so default to
	the second one; for a single-argument method the only argument is removable."

	^ methodName arguments size <= 1
		  ifTrue: [ 1 ]
		  ifFalse: [ 2 ]
]

{ #category : 'accessing' }
StMethodArgumentsRemoverPresenter >> defaultSelectorAfterRemoving: argumentName [

	| argIndex keywords |
	argIndex := methodName arguments indexOf: argumentName.
	keywords := methodName selector keywords asOrderedCollection.
	keywords size = 1 ifTrue: [ ^ (keywords first copyWithout: $:) asSymbol ].
	keywords removeAt: argIndex.
	^ (keywords joinUsing: '') asSymbol
]

{ #category : 'accessing' }
StMethodArgumentsRemoverPresenter >> getParametersOrder [

	^ argumentsList items 
		reject: [ :arg | arg = argumentsList selection selectedItem ] 
		thenCollect: [ :arg | arg newName ]
]

{ #category : 'accessing' }
StMethodArgumentsRemoverPresenter >> inform: aString [

	self application inform: aString
]

{ #category : 'initialization' }
StMethodArgumentsRemoverPresenter >> initializeDialogWindow: aModalPresenter [

	aModalPresenter
		initialExtent: 300 @ 300;
		title: 'Remove argument';
		addButton: 'Cancel' do: [ :presenter | presenter beCancel; close ];
		addDefaultButton: 'Accept' do: [ :presenter | self renameMethodAndClose: presenter ]
]

{ #category : 'initialization' }
StMethodArgumentsRemoverPresenter >> initializePresenters [

	selectorInput := self newTextInput.
	argumentsList := self newList.
	previewResult := self newLabel.

	selectorInput text: methodName selector.
	previewResult label: methodName methodName
]

{ #category : 'accessing' }
StMethodArgumentsRemoverPresenter >> invalidArgNames [

	^ invalidArgNames ifNil: [ invalidArgNames := {  } ]
]

{ #category : 'accessing' }
StMethodArgumentsRemoverPresenter >> invalidArgNames: aSet [

	invalidArgNames := aSet
]

{ #category : 'accessing' }
StMethodArgumentsRemoverPresenter >> methodName [

	^ methodName
]

{ #category : 'accessing' }
StMethodArgumentsRemoverPresenter >> previewLabel [

	| selectedIndex |
	selectedIndex := argumentsList selection selectedIndex.
	(self canRemoveArgumentAt: selectedIndex) ifFalse: [ ^ '(invalid)' ].
	^ (RBMethodName
		   selector: (selectorInput text ifNil: [ '' ])
		   arguments: (argumentsList items reject: [ :arg | argumentsList selection selectedItem = arg ] thenCollect: #newName))
		  methodName
]

{ #category : 'accessing' }
StMethodArgumentsRemoverPresenter >> previewResult [

	^ previewResult
]

{ #category : 'actions' }
StMethodArgumentsRemoverPresenter >> renameArgument: anItem [

	| argName selectedIndex |
	selectedIndex := argumentsList selection selectedIndex.
	argName := self request: 'Enter default name of argument' initialAnswer: anItem asString.
	argName ifNil: [ CmdCommandAborted signal ].
	(self invalidArgNames includes: argName) ifTrue: [
			self inform: 'It''s a invalid name'.
			CmdCommandAborted signal ].
	anItem newName: argName.
	argumentsList items: (argumentsList items copyUpTo: anItem) , { anItem } , (argumentsList items copyAfter: anItem).
	argumentsList selectIndex: selectedIndex.
	self updateLabel
]

{ #category : 'accessing' }
StMethodArgumentsRemoverPresenter >> renameMap [

	^ (argumentsList items reject: [ :arg | arg = argumentsList selection selectedItem ]) select: [ :arg | arg hasNewName ]
]

{ #category : 'actions' }
StMethodArgumentsRemoverPresenter >> renameMethodAndClose: presenter [

	| selectedIndex |
	selectedIndex := argumentsList selection selectedIndex.
	(self canRemoveArgumentAt: selectedIndex) ifFalse: [ ^ self inform: 'The first argument (method name) cannot be removed.' ].
	^ previewResult label = '(invalid)'
		  ifTrue: [ self inform: 'Invalid method name' ]
		  ifFalse: [
				  methodName
					  arguments: self getParametersOrder;
					  selector: selectorInput text;
					  renameMap: self renameMap.
				  presenter
					  beOk;
					  close ]
]

{ #category : 'accessing' }
StMethodArgumentsRemoverPresenter >> request: aString initialAnswer: aSecondString [

	^ requestor
		  ifNil: [
				  self application newRequest
					  title: 'Information needed';
					  message: aString;
					  text: aSecondString;
					  openModal ]
		  ifNotNil: [ requestor label ]
]

{ #category : 'accessing' }
StMethodArgumentsRemoverPresenter >> requestor: aMockObject [

	requestor := aMockObject
]

{ #category : 'accessing' }
StMethodArgumentsRemoverPresenter >> selectorInput [

	^ selectorInput
]

{ #category : 'initialization' }
StMethodArgumentsRemoverPresenter >> setModelBeforeInitialization: aRBMethodName [

	methodName := aRBMethodName.
	args := methodName arguments copy
]

{ #category : 'updating' }
StMethodArgumentsRemoverPresenter >> updateLabel [
	"Update the new method name to display to the user when the user change 
	 its name or order of the arguments."

	previewResult label: self previewLabel
]
