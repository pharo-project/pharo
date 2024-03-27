"
It provides a request dialog to add a new class in the system, including presenters to specify its name, package tag and comment.

"
Class {
	#name : 'StRefactoringAddClassPresenter',
	#superclass : 'SpRequestDialog',
	#instVars : [
		'newClassPresenter',
		'packagePresenter',
		'tagPresenter',
		'commentPresenter',
		'sycCmAddSubclassCommand',
		'useTemplateCheckPresenter'
	],
	#category : 'Refactoring-UI-UI',
	#package : 'Refactoring-UI',
	#tag : 'UI'
}

{ #category : 'examples' }
StRefactoringAddClassPresenter class >> example [
	<script>
	
	self new openModal
]

{ #category : 'layout' }
StRefactoringAddClassPresenter >> defaultLayout [ 

	^ SpGridLayout new
		  add: 'New class name' at: 1 @ 1;
		  add: newClassPresenter at: 2 @ 1 span: 2 @ 1;
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
StRefactoringAddClassPresenter >> initialExtentForWindow [

	^ (500 @ 450) scaledByDisplayScaleFactor
]

{ #category : 'initialization' }
StRefactoringAddClassPresenter >> initializeClassNamePresenter [

	newClassPresenter := self newTextInput
		whenTextChangedDo: [ : text | 
			(Smalltalk globals hasClassNamed: text asSymbol)
				ifTrue: [ self inform: 'Class ' , text asString , ' already exists' ]
				ifFalse: [ sycCmAddSubclassCommand newClassName: self newClassName ] ];
		yourself.
]

{ #category : 'initialization' }
StRefactoringAddClassPresenter >> initializeCommentPresenter [

	useTemplateCheckPresenter := self newCheckBox
		state: false;
		help: 'When checked, the comment will be prefilled with a generic class comment';
		label: 'Use comment template';
		whenActivatedDo: [ commentPresenter text: sycCmAddSubclassCommand targetClass classCommentBlank  ];
		whenDeactivatedDo: [ commentPresenter text: String empty ];
		yourself.
	commentPresenter := self newText
		beWrapWord;
		placeholder: 'A comment for your new class';
		whenTextChangedDo: [ : text | sycCmAddSubclassCommand comment: commentPresenter text trimmed ];
		yourself.
]

{ #category : 'initialization' }
StRefactoringAddClassPresenter >> initializeFocus [

	self focusOrder 
		add: newClassPresenter;
		add: packagePresenter;
		add: tagPresenter;
		add: commentPresenter
]

{ #category : 'initialization' }
StRefactoringAddClassPresenter >> initializePackagesPresenter [

	packagePresenter := self newDropList
		startWithSelection;
		items: sycCmAddSubclassCommand packageNames;
		displayIcon: [ : aPackage | self iconNamed: aPackage systemIconName ];
		sortingBlock: [ : a : b | a model < b model ];
		whenSelectedItemChangedDo: [ : item | 
			self updateTagItems: item.		
			self updateTagSelection: item ];
		selectItem: sycCmAddSubclassCommand targetPackageName;		
		yourself.
]

{ #category : 'initialization' }
StRefactoringAddClassPresenter >> initializePresenters [ 

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

	self setTitleTo: aSpWindowPresenter.
	self setWindowIconTo: aSpWindowPresenter.
	self setInitialExtentTo: aSpWindowPresenter.
	aSpWindowPresenter aboutText: String empty
]

{ #category : 'instance creation' }
StRefactoringAddClassPresenter >> newClassName [
	"Answer a <String> containing the current contents of the new class name presenter"
	
	^ newClassPresenter text trimmed
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

{ #category : 'initialization' }
StRefactoringAddClassPresenter >> setInitialExtentTo: aSpWindowPresenter [

	aSpWindowPresenter initialExtent: self initialExtentForWindow
]

{ #category : 'accessing - model' }
StRefactoringAddClassPresenter >> setModelBeforeInitialization: aSycCmAddSubclassCommand [

	sycCmAddSubclassCommand := aSycCmAddSubclassCommand
]

{ #category : 'initialization' }
StRefactoringAddClassPresenter >> setTitleTo: aSpWindowPresenter [

	aSpWindowPresenter title: 'Add new subclass of ' , sycCmAddSubclassCommand targetClass name
]

{ #category : 'initialization' }
StRefactoringAddClassPresenter >> setWindowIconTo: aSpWindowPresenter [

	aSpWindowPresenter windowIcon: (self iconNamed: #add)
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

	sycCmAddSubclassCommand tagName: tagPresenter selectedItem.
]

{ #category : 'private' }
StRefactoringAddClassPresenter >> validateAnswer [
	
	validateBlock ifNotNil: [ 
		^ validateBlock 
			value: newClassPresenter text
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
