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
		'commentPresenter',
		'sycCmAddSubclassCommand'
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

	newClassPresenter := self newTextInput
		whenTextChangedDo: [ : text | sycCmAddSubclassCommand newClassName: self newClassName ];
		yourself.
	packagePresenter := self newDropList
		items: PackageOrganizer default packageNames;
		whenSelectedItemChangedDo: [ : item | 
			self updateTagItems: item.		
			self updateTagSelection: item ];
		yourself.
	tagPresenter := self newDropList
		whenSelectedItemChangedDo: [ : item |
			self updateTagSelection: item ];
		yourself.
	commentPresenter := self newText
		whenTextChangedDo: [ : text | sycCmAddSubclassCommand comment: commentPresenter text trimmed ];
		yourself.
	
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
	| dialog |
	
	dialog := self asModalWindow open; centered.
	^ dialog isOk
		ifTrue: [ self ]
		ifFalse: [ nil ]
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

	aSpWindowPresenter title: 'Add new class'
]

{ #category : 'initialization' }
StRefactoringAddClassPresenter >> setWindowIconTo: aSpWindowPresenter [

	aSpWindowPresenter windowIcon: (self iconNamed: #add)
]

{ #category : 'initialization' }
StRefactoringAddClassPresenter >> updateTagItems: item [

	| pkg |
	
	pkg := PackageOrganizer default packageNamed: item.
	tagPresenter items: pkg tagNames asOrderedCollection.	
]

{ #category : 'initialization' }
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
