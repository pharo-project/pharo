"
A ListSelectionPresenter is a basic example to show multi selection on lists

self example
"
Class {
	#name : #SpListSelectionPresenter,
	#superclass : #SpPresenter,
	#instVars : [
		'listModel',
		'textModel1',
		'textModel2',
		'textRefreshingProcess'
	],
	#category : #'Spec2-Examples-Standalone'
}

{ #category : #specs }
SpListSelectionPresenter class >> defaultSpec [
	^ SpPanedLayout newVertical
		add: #listModel;
		add:
			(SpPanedLayout newHorizontal
				add: #textModel1;
				add: #textModel2;
				yourself);
		yourself
]

{ #category : #example }
SpListSelectionPresenter class >> example [
	<sampleInstance>
	^ self new openWithSpec
]

{ #category : #initialization }
SpListSelectionPresenter >> connectPresenters [
	listModel whenSelectionChangedDo: [ self updateText ]
]

{ #category : #initialization }
SpListSelectionPresenter >> initializePresenters [
	listModel := self newList.
	textModel1 := self newText.
	textModel2 := self newCode.
	listModel beMultipleSelection.
	listModel items: self class environment allClasses
]

{ #category : #accessing }
SpListSelectionPresenter >> listModel [

	^ listModel
]

{ #category : #accessing }
SpListSelectionPresenter >> textModel1 [

	^ textModel1
]

{ #category : #accessing }
SpListSelectionPresenter >> textModel2 [

	^ textModel2
]

{ #category : #update }
SpListSelectionPresenter >> updateText [
	"update the displayed list in a separate thread to avoid UI blocking"

	"if there is already a background thread running for the new list discard it"

	textRefreshingProcess ifNotNil: [ textRefreshingProcess terminate ].

	"fork off a possibly costly list calculation"
	textRefreshingProcess := [ | indexes items |
	indexes := listModel selection selectedIndexes sort
		collect: [ :i | i printString ].
	indexes := indexes joinUsing: '; '.
	items := listModel selectedItems collect: [ :i | i printString ].
	items := items sort joinUsing: '; '.
	UIManager default
		defer: [ textModel1 text: indexes.
			textModel2 text: items ] ] fork
]
