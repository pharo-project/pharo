"
It provides a request dialog to add a new class in the system, including presenters to specify its name, package tag and comment.
Pay attention that the name of the new class is set in the subclass instance variable of the model (driver for example).

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
StRefactoringAddClassPresenter class >> defaultPreferredExtent [

	^ 500 @ 450
]

{ #category : 'accessing' }
StRefactoringAddClassPresenter >> commentPresenterText [

	^ commentPresenter text
]

{ #category : 'layout' }
StRefactoringAddClassPresenter >> defaultLayout [

	^ SpGridLayout new
		beColumnNotHomogeneous;
		beRowNotHomogeneous;
		column: 2 expand: true;
		add: 'New class name' atPoint: 1 @ 1;
		add: textInput atPoint: 2 @ 1;
		add: 'Package' atPoint: 1 @ 2;
		add: packagePresenter atPoint: 2 @ 2;
		add: 'Tag' atPoint: 1 @ 3;
		add: tagPresenter atPoint: 2 @ 3;
		add: 'Comment' atPoint: 1 @ 4;
		add: (SpBoxLayout newTopToBottom
		  		add: useTemplateCheckPresenter expand: false;
				add: commentPresenter;
				yourself)
			atPoint: 2 @ 4;
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
		whenActivatedDo: [ commentPresenter text: driver superclass realClass  classCommentBlank ];
		whenDeactivatedDo: [ commentPresenter text: String empty ];
		yourself.
	commentPresenter := self newText
		addStyle: 'lines8';
		beWrapWord;
		placeholder: 'A comment for your new class';
		whenTextChangedDo: [ :text | driver comment: commentPresenter text trimBoth ];
		yourself
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
		initializeFocus
]

{ #category : 'showing' }
StRefactoringAddClassPresenter >> open [

	super
		open;
		centered
]

{ #category : 'callbacks' }
StRefactoringAddClassPresenter >> updateTagItems: item [
	"Note that as it seems there is no Spec drop list with editable text, we provide a 'Core' tag by default if the package has no tags"

	| pkg superclassTag |
	superclassTag := driver superclass packageTagName.
	item
		ifNil: [
				superclassTag
					ifNotNil: [ tagPresenter items: { superclassTag } ]
					ifNil: [ tagPresenter items: { 'Core' } ] ]
		ifNotNil: [
				(pkg := PackageOrganizer default packageNamed: item) tagNames
					ifEmpty: [
							superclassTag
								ifNotNil: [ tagPresenter items: { superclassTag } ]
								ifNil: [ tagPresenter items: { 'Core' } ] ]
					ifNotEmpty: [ :tagNames |
							| orderedTags |
							orderedTags := tagNames asOrderedCollection.
							superclassTag ifNotNil: [
									(orderedTags includes: superclassTag) ifTrue: [
											orderedTags remove: superclassTag.
											orderedTags addFirst: superclassTag ] ].
							tagPresenter items: orderedTags ] ].
	tagPresenter selectIndex: 1
]

{ #category : 'accessing' }
StRefactoringAddClassPresenter >> windowIcon [

	^ self iconNamed: #add
]
