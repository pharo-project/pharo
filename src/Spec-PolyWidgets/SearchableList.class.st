"
SearchableList 
	new items: #( aaa abc axz bbb bfbg bof c);
	openWithSpec
	
should it be a subclass of ListPresenter?
Because now I will have to delegate a lot? and I cannot plug a IconList

How do I make textModel react to every keystroke without an accept
"
Class {
	#name : #SearchableList,
	#superclass : #ComposablePresenter,
	#instVars : [
		'listPresenter',
		'searchPresenter'
	],
	#category : #'Spec-PolyWidgets-ListAndTree'
}

{ #category : #specs }
SearchableList class >> defaultSpec [

	^ SpecLayout composed
		newRow: #searchPresenter height: self toolbarHeight;
		add: #listPresenter top: self toolbarHeight;
		yourself
]

{ #category : #example }
SearchableList class >> example [
	<sampleInstance>
	"self example"

	^ self new
		items: #(#aaa #abc #axz #bbb #bfbg #bof #c); openWithSpec
]

{ #category : #initialization }
SearchableList >> initializePresenter [

	searchPresenter whenTextChanged: [:newText || text |
		text :=  searchPresenter getText asLowercase.
		text isEmpty
			ifTrue: [ listPresenter resetFilteringBlock ]
			ifFalse: [
				listPresenter filteringBlock: [:col | col select: [:e | e asLowercase beginsWith: searchPresenter getText asLowercase ]]]]
]

{ #category : #initialization }
SearchableList >> initializeWidgets [

	listPresenter := self newList.
	searchPresenter := self newTextInput.
		
	searchPresenter 
		autoAccept: true;
		ghostText: 'filter'.
]

{ #category : #accessing }
SearchableList >> items: aCollection [
	listPresenter items: aCollection
]

{ #category : #accessing }
SearchableList >> listPresenter [
	^ listPresenter
]

{ #category : #accessing }
SearchableList >> searchPresenter [
	^ searchPresenter
]
