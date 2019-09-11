"
I am a preview to accept or denegate a replacement an ocurrence of method (Find and replace refactoring).

Instance Variables
------------------------------------------

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
	#category : #'Refactoring-Core'
}

{ #category : #initialization }
RBReplacePreview class >> change: anObject [

	^ self new
		change: anObject;
		yourself
]

{ #category : #specs }
RBReplacePreview class >> defaultSpec [
	<spec: #default>
	
	^ SpBoxLayout newVertical 
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
		[ :aChange | RBRefactoryChangeManager instance performChange: aChange ] 
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
	aCompositeChange := RBRefactoryChangeManager changeFactory compositeRefactoryChange .
	aCompositeChange addChange: anObject .
	
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

{ #category : #'instance creation' }
RBReplacePreview >> open [
	^ self "openDialogWithSpec"openModalWithSpec 
]

{ #category : #initialization }
RBReplacePreview >> replaceAllAction: aBlock [ 
	replaceAllAction := aBlock
]

{ #category : #initialization }
RBReplacePreview >> skipAction: aBlock [ 
	skipAction := aBlock
]
