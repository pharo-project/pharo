"
It provides a request dialog to add a new class in the system, including presenters to specify its name, package tag and comment.

"
Class {
	#name : 'StRefactoringAddClassPresenter',
	#superclass : 'SpRequestDialog',
	#classTraits : '{} + TraitedClass',
	#instVars : [
		'newClassPresenter',
		'packagePresenter',
		'tagPresenter',
		'commentPresenter'
	],
	#category : 'Refactoring-UI-UI',
	#package : 'Refactoring-UI',
	#tag : 'UI'
}

{ #category : 'examples' }
StRefactoringAddClassPresenter class >> example [

	| presenter |
	presenter := self new.
	presenter openDialog
]

{ #category : 'initialization' }
StRefactoringAddClassPresenter >> connectPresenters [ 

	packagePresenter 
		transmitTo: tagPresenter 
		transform: [ : item |
			| pkg |
			pkg := PackageOrganizer default packageNamed: item label.
			tagPresenter items: pkg tagNames asOrderedCollection ]
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
		  add: 'Coment' at: 1 @ 4;
		  add: commentPresenter at: 2 @ 4 span: 2 @ 1;
		  beColumnNotHomogeneous;
		  yourself
		
]

{ #category : 'initialization' }
StRefactoringAddClassPresenter >> initialExtentForWindow [

	^ (500 @ 250) scaledByDisplayScaleFactor
]

{ #category : 'initialization' }
StRefactoringAddClassPresenter >> initializePresenters [ 

	newClassPresenter := self newTextInput.
	packagePresenter := self newDropList
		items: PackageOrganizer default packageNames;
		yourself.
	tagPresenter := self newDropList.
	commentPresenter := self newText.
	
	self 
		acceptLabel: 'Create';
		validateAnswer: self validateAnswerBlock
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
	
	^ newClassPresenter text
]

{ #category : 'showing' }
StRefactoringAddClassPresenter >> open [

	super
		open;
		centered
]

{ #category : 'api - showing' }
StRefactoringAddClassPresenter >> openModal [
	| dialog |
	
	dialog := self asModalWindow open; centered.
	^ dialog isOk
		ifTrue: [ self newClassName trimmed ]
		ifFalse: [ nil ]
]

{ #category : 'initialization' }
StRefactoringAddClassPresenter >> setInitialExtentTo: aSpWindowPresenter [

	aSpWindowPresenter initialExtent: self initialExtentForWindow
]

{ #category : 'initialization' }
StRefactoringAddClassPresenter >> setTitleTo: aSpWindowPresenter [

	aSpWindowPresenter title: 'Add new class'
]

{ #category : 'initialization' }
StRefactoringAddClassPresenter >> setWindowIconTo: aSpWindowPresenter [

	aSpWindowPresenter windowIcon: (self iconNamed: #add)
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
