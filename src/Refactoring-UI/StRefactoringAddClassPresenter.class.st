"
It provides a request dialog to add a new class in the system, including presenters to specify its name, package tag and comment.

"
Class {
	#name : 'StRefactoringAddClassPresenter',
	#superclass : 'StRequestClassPresenter',
	#instVars : [
		'commentPresenter',
		'useTemplateCheckPresenter'
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
StRefactoringAddClassPresenter >> initializePresenters [

	super initializePresenters.
	self initializeCommentPresenter.

	self
		acceptLabel: 'Create';
		validateAnswer: self validateAnswerBlock;
		initializeFocus
]

{ #category : 'showing' }
StRefactoringAddClassPresenter >> open [

	super
		open;
		centered
]

{ #category : 'initialization' }
StRefactoringAddClassPresenter >> setTitleTo: aSpWindowPresenter [

	aSpWindowPresenter title: 'Add new subclass of ' , driver superclass name
]

{ #category : 'initialization' }
StRefactoringAddClassPresenter >> setWindowIconTo: aSpWindowPresenter [

	aSpWindowPresenter windowIcon: (self iconNamed: #add)
]
