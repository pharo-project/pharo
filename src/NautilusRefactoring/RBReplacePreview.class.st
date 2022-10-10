"
I am a preview tool to accept or denegate a replacement an occurrence of method (Find and replace refactoring).
I was packaged in Refactoring-Core and it was not an ideal place.
I'm not used for now. I was in general open in modal mode.

diffPresenter:    <SpDiffPresenter> A diff presenter to show the users the consequences of the refactorings.
change:           <Change> A  refactoring change
skipAction:       <Block> A  block with an action when press skip button
replaceAllAction: <Block> A  block with an action when press replace all button
"
Class {
	#name : #RBReplacePreview,
	#superclass : #SpPresenter,
	#instVars : [
		'diffPresenter',
		'change',
		'skipAction',
		'replaceAllAction',
		'cancelAction'
	],
	#category : #NautilusRefactoring
}

{ #category : #initialization }
RBReplacePreview class >> change: anObject [

	^ self new
		change: anObject;
		yourself
]

{ #category : #layout }
RBReplacePreview class >> defaultLayout [
	
	^ SpBoxLayout newTopToBottom 
				add: #diffPresenter;
				yourself
]

{ #category : #specs }
RBReplacePreview class >> title [ 
	^ 'Refactoring Preview'
]

{ #category : #initialization }
RBReplacePreview >> accept: presenter [
	
	self okToChange
		ifFalse: [ ^ self ].
	[ change do: 
		[ :aChange | RBRefactoryChangeManager instance 
			performChange: aChange;
			addUndoPointer: (RBRefactoryChangeManager nextCounter) ] 
	] asJob
		title: 'Refactoring';
		run.
	presenter
				beOk;
				close
]

{ #category : #private }
RBReplacePreview >> buildDiffFor: aChange [
	diffPresenter  
		leftText: aChange oldVersionTextToDisplay;
		rightText: aChange textToDisplay
]

{ #category : #initialization }
RBReplacePreview >> cancelAction: aBlock [ 
	cancelAction := aBlock
]

{ #category : #initialization }
RBReplacePreview >> change: anObject [
	|aCompositeChange |
	aCompositeChange := RBRefactoryChangeManager changeFactory compositeRefactoryChange.
	aCompositeChange addChange: anObject.
	
	change := aCompositeChange whatToDisplayIn: self
]

{ #category : #initialization }
RBReplacePreview >> initializeDialogWindow: aModalPresenter [
	aModalPresenter
		initialExtent: 700 @ 500;
		
		addButton: 'Cancel' do: cancelAction;
		addButton: 'Replace' do: [ :presenter | self accept: presenter ];
		addButton: 'Skip' do: skipAction;
		addButton: 'Replace all' do: replaceAllAction
]

{ #category : #initialization }
RBReplacePreview >> initializeWidgets [
	diffPresenter := self newDiff showOptions: false; contextClass: True.
	self whenBuiltDo: [ self buildDiffFor: change first ]
]

{ #category : #initialization }
RBReplacePreview >> replaceAllAction: aBlock [ 
	replaceAllAction := aBlock
]

{ #category : #initialization }
RBReplacePreview >> skipAction: aBlock [ 
	skipAction := aBlock
]
