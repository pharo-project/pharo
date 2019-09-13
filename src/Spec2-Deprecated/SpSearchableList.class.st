"
SearchableList 
	new items: #( aaa abc axz bbb bfbg bof c);
	openWithSpec
	
should it be a subclass of ListPresenter?
Because now I will have to delegate a lot? and I cannot plug a IconList

How do I make textModel react to every keystroke without an accept
"
Class {
	#name : #SpSearchableList,
	#superclass : #SpPresenter,
	#instVars : [
		'listPresenter',
		'searchPresenter',
		'baseItems'
	],
	#category : #'Spec2-Deprecated-Replaced'
}

{ #category : #specs }
SpSearchableList class >> defaultSpec [
	^ SpBoxLayout newVertical
		add: #searchPresenter withConstraints: [ :constraints | constraints height: self toolbarHeight ];
		add: #listPresenter;
		yourself
]

{ #category : #examples }
SpSearchableList class >> example [
	<sampleInstance>
	"self example"

	^ self new
		items: #(#aaa #abc #axz #bbb #bfbg #bof #c); openWithSpec
]

{ #category : #deprecation }
SpSearchableList class >> isDeprecated [
	"Now lists have a filter integrated in them."

	^ true
]

{ #category : #initialization }
SpSearchableList >> connectPresenters [
	searchPresenter
		whenTextChangedDo: [ :newText | 
			searchPresenter text asLowercase
				ifEmpty: [ listPresenter items: baseItems ]
				ifNotEmpty: [ :text | 
					listPresenter
						items:
							(baseItems select: [ :element | element asLowercase beginsWith: text ]) ] ]
]

{ #category : #initialization }
SpSearchableList >> initializePresenters [
	listPresenter := self newList.
	searchPresenter := self newTextInput.
	searchPresenter
		autoAccept: true;
		placeholder: 'Filter'
]

{ #category : #accessing }
SpSearchableList >> items: aCollection [
	baseItems := aCollection.
	listPresenter items: aCollection
]

{ #category : #accessing }
SpSearchableList >> listPresenter [
	^ listPresenter
]

{ #category : #accessing }
SpSearchableList >> searchPresenter [
	^ searchPresenter
]
