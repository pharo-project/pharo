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

{ #category : 'initialization' }
StMethodArgumentsRemoverPresenter >> connectPresenters [

	selectorInput whenTextChangedDo: [ :text | self updateLabel ].
	argumentsList whenSelectionChangedDo: [ :selection |
		selection selectedItem ifNotNil: [ selectorInput text: (self defaultSelectorAfterRemoving: selection selectedItem name) ] ].
	argumentsList displayColor: [ :arg |
			arg = argumentsList selection selectedItem
				ifTrue: [ Color lightGray ]
				ifFalse: [ self theme textColor ] ].
	argumentsList
		items: (methodName arguments collect: [ :arg | RBArgumentName name: arg ]);
		actions: self argumentsListActions.
	argumentsList items ifNotEmpty: [ argumentsList selectIndex: 1 ]
]

{ #category : 'layout' }
StMethodArgumentsRemoverPresenter >> defaultLayout [

	^ SpBoxLayout newTopToBottom
		add: (SpBoxLayout newTopToBottom
			add: 'Argument to be removed';
			add: argumentsList;
			yourself);
		add: 'Modified selector';
		add: selectorInput;
		add: 'Preview';
		add: previewResult;
		yourself
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

	^ argumentsList items reject: [ :arg | arg = argumentsList selection selectedItem ] thenCollect: [ :arg | arg newName ]
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
		addButton: 'Cancel' do: [ :presenter |
				presenter
					beCancel;
					close ];
		addDefaultButton: 'Accept' do: [ :presenter | self renameMethodAndClose: presenter ]
]

{ #category : 'initialization' }
StMethodArgumentsRemoverPresenter >> initializePresenters [

	selectorInput := self newTextInput.
	argumentsList := self newList.
	previewResult := self newLabel.

	selectorInput autoAccept: true.
	selectorInput text: methodName selector.
	previewResult label: methodName methodName.

	self setFocus
]

{ #category : 'accessing' }
StMethodArgumentsRemoverPresenter >> invalidArgNames [

	^ invalidArgNames ifNil: [ invalidArgNames := {  } ]
]

{ #category : 'accessing' }
StMethodArgumentsRemoverPresenter >> invalidArgNames: aSet [

	invalidArgNames := aSet
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

{ #category : 'initialization' }
StMethodArgumentsRemoverPresenter >> setFocus [

	self focusOrder
		add: argumentsList;
		add: selectorInput
]

{ #category : 'initialization' }
StMethodArgumentsRemoverPresenter >> setModelBeforeInitialization: aRBMethodName [

	methodName := aRBMethodName.
	args := methodName arguments copy
]

{ #category : 'updating' }
StMethodArgumentsRemoverPresenter >> updateLabel [
	"Update the new method name to display to the user when the user change its name or order of the arguments."

	previewResult label: (RBMethodName
			 selector: (selectorInput text ifNil: [ '' ])
			 arguments: (argumentsList items reject: [ :arg | argumentsList selection selectedItem = arg ] thenCollect: #newName))
			methodName
]
