"
It provides a request dialog to add a new class in the system, including presenters to specify its name, package tag and comment.

"
Class {
	#name : 'StRefactoringAddClassPresenter',
	#superclass : 'SpRequestDialog',
	#instVars : [
		'packagePresenter',
		'tagPresenter',
		'commentPresenter',
		'useTemplateCheckPresenter',
		'driver'
	],
	#category : 'Refactoring-UI-UI',
	#package : 'Refactoring-UI',
	#tag : 'UI'
}

{ #category : 'accessing' }
StRefactoringAddClassPresenter class >> defaultExtent [

	^ (500 @ 450) scaledByDisplayScaleFactor
]

{ #category : 'accessing' }
StRefactoringAddClassPresenter >> commentPresenterText [

	^ commentPresenter text
]

{ #category : 'layout' }
StRefactoringAddClassPresenter >> defaultLayout [ 

	^ SpGridLayout new
		  add: 'New class name' at: 1 @ 1;
		  add: textInput at: 2 @ 1 span: 2 @ 1;
		  add: 'Package' at: 1 @ 2;
		  add: packagePresenter at: 2 @ 2 span: 2 @ 1;
		  add: 'Tag' at: 1 @ 3;
		  add: tagPresenter at: 2 @ 3 span: 2 @ 1;
		  add: 'Comment' at: 1 @ 4;
		  add: (SpBoxLayout newTopToBottom
				add: useTemplateCheckPresenter expand: false;
				add: commentPresenter withConstraints: [ : c | c height: 500 ];
				yourself) at: 2 @ 4 span: 2 @ 1;
		  beColumnNotHomogeneous;
		  yourself
		
]

{ #category : 'initialization' }
StRefactoringAddClassPresenter >> initializeClassNamePresenter [

	textInput := self newTextInput
		whenTextChangedDo: [ : text | 
			(Smalltalk globals hasClassNamed: text asSymbol)
				ifTrue: [ self inform: 'Class ' , text asString , ' already exists' ]
				ifFalse: [ driver subclass: self newClassName ] ];
		yourself.
]

{ #category : 'initialization' }
StRefactoringAddClassPresenter >> initializeCommentPresenter [

	useTemplateCheckPresenter := self newCheckBox
		state: false;
		help: 'When checked, the comment will be prefilled with a generic class comment';
		label: 'Use comment template';
		whenActivatedDo: [ commentPresenter text: driver superclass classCommentBlank  ];
		whenDeactivatedDo: [ commentPresenter text: String empty ];
		yourself.
	commentPresenter := self newText
		beWrapWord;
		placeholder: 'A comment for your new class';
		whenTextChangedDo: [ : text | driver comment: commentPresenter text trimmed ];
		yourself.
]

{ #category : 'initialization' }
StRefactoringAddClassPresenter >> initializeFocus [

	self focusOrder 
		add: textInput;
		add: packagePresenter;
		add: tagPresenter;
		add: commentPresenter
]

{ #category : 'initialization' }
StRefactoringAddClassPresenter >> initializePackagesPresenter [

	packagePresenter := self newDropList
		startWithSelection;
		items: driver packageNames;
		displayIcon: [ : aPackage | self iconNamed: aPackage systemIconName ];
		sortingBlock: [ : a : b | a model < b model ];
		whenSelectedItemChangedDo: [ : item | 
			self updateTagItems: item.		
			self updateTagSelection: item ];
		selectItem: driver packageName;
		yourself.

]

{ #category : 'initialization' }
StRefactoringAddClassPresenter >> initializePresenters [ 

	super initializePresenters.
	self
		initializeClassNamePresenter;
		initializeTagsPresenter;
		initializePackagesPresenter;
		initializeCommentPresenter.
	
	self 
		acceptLabel: 'Create';
		validateAnswer: self validateAnswerBlock;
		initializeFocus.
]

{ #category : 'initialization' }
StRefactoringAddClassPresenter >> initializeTagsPresenter [

	tagPresenter := self newDropList
		startWithSelection;
		whenSelectedItemChangedDo: [ : item |
			self updateTagSelection: item ];
		yourself.
]

{ #category : 'initialization' }
StRefactoringAddClassPresenter >> initializeWindow: aSpWindowPresenter [

	super initializeWindow: aSpWindowPresenter.
	self setTitleTo: aSpWindowPresenter.
	self setWindowIconTo: aSpWindowPresenter.
	aSpWindowPresenter aboutText: String empty
]

{ #category : 'accessing' }
StRefactoringAddClassPresenter >> newClassName [
	"Answer a <String> containing the current contents of the new class name presenter"
	
	^ textInput text trimmed
]

{ #category : 'showing' }
StRefactoringAddClassPresenter >> open [

	super
		open;
		centered
]

{ #category : 'api - showing' }
StRefactoringAddClassPresenter >> openModal [
	
	^ self asModalWindow open; centered.
]

{ #category : 'accessing' }
StRefactoringAddClassPresenter >> packageName [

	^ packagePresenter selectedItem
]

{ #category : 'accessing - model' }
StRefactoringAddClassPresenter >> setModelBeforeInitialization: aRBInteractionDriver [

	driver := aRBInteractionDriver
]

{ #category : 'initialization' }
StRefactoringAddClassPresenter >> setTitleTo: aSpWindowPresenter [

	aSpWindowPresenter title: 'Add new subclass of ' , driver superclass name
]

{ #category : 'initialization' }
StRefactoringAddClassPresenter >> setWindowIconTo: aSpWindowPresenter [

	aSpWindowPresenter windowIcon: (self iconNamed: #add)
]

{ #category : 'accessing' }
StRefactoringAddClassPresenter >> tagName [

	^ tagPresenter selectedItem
]

{ #category : 'callbacks' }
StRefactoringAddClassPresenter >> updateTagItems: item [
	"Note that as it seems there is no Spec drop list with editable text, we provide a 'Core' tag by default if the package has no tags"
	| pkg |
	
	pkg := PackageOrganizer default packageNamed: item.
	pkg tagNames
		ifEmpty: [ tagPresenter items: { 'Core' }  ]
		ifNotEmpty: [ : tagNames | tagPresenter items: tagNames asOrderedCollection ].
	tagPresenter selectIndex: 1
]

{ #category : 'callbacks' }
StRefactoringAddClassPresenter >> updateTagSelection: item [

	driver tagName: tagPresenter selectedItem.
]

{ #category : 'private' }
StRefactoringAddClassPresenter >> validateAnswer [
	
	validateBlock ifNotNil: [ 
		^ validateBlock 
			value: textInput text
			value: packagePresenter selectedItem
			value: tagPresenter selectedItem ].
	^ true
]

{ #category : 'private' }
StRefactoringAddClassPresenter >> validateAnswerBlock [

	^ [ : newClassName : packageName : tagName |
		  | isValid |
		  isValid := (self validateNameBlock value: newClassName) and: [ (self validateNameBlock value: packageName) and: [ self validateNameBlock value: tagName ] ].
		  isValid 
			ifFalse: [ self inform: 'Name can''t be empty or contain spaces' ].
		  isValid ]
]

{ #category : 'private' }
StRefactoringAddClassPresenter >> validateNameBlock [

	^ [ :txt |
	  | isValid |
	  isValid := txt isNotNil and: [
		             txt isNotEmpty and: [ (txt includesSubstring: ' ') not ] ].
	  isValid ]
]
