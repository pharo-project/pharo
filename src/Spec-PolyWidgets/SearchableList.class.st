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
		'searchPresenter',
		'baseItems'
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

{ #category : #initialization }
SearchableList >> initializePresenter [
	searchPresenter
		whenTextChangedDo:
			[ :newText |
			searchPresenter getText asLowercase
				ifEmpty: [ listPresenter items: baseItems ]
				ifNotEmpty: [ :text | listPresenter items: (baseItems select: [ :element | element asLowercase beginsWith: text ]) ] ]
]

{ #category : #initialization }
SearchableList >> initializeWidgets [
	listPresenter := self newList.
	searchPresenter := self newTextInput.
	searchPresenter
		autoAccept: true;
		placeholder: 'Filter'
]

{ #category : #accessing }
SearchableList >> items: aCollection [
	baseItems := aCollection.
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
