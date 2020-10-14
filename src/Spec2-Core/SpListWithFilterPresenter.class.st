"
I am a presenter that has a list and a text input for filtering the elements on the list.

The list items are filtered based on what the `display` method of the list returns. In other words, what you see is what is used to filter.

I have two predefined filter methods: `self enableItemBeginsWithFilter` and `self enableItemSubstringFilter`. You can set your own method be calling `self itemFilter: aBlock` with a block that has two parameters: the item and the pattern. See the definitions of the above methods for details.

I have two subwidgets:
- A List (`SpListPresenter`) that has the items.
- A Text Input Field (`SpTextInputFieldPresenter`) for entering the filter text.
"
Class {
	#name : #SpListWithFilterPresenter,
	#superclass : #SpPresenter,
	#instVars : [
		'itemFilter',
		'unfilteredItems',
		'list',
		'filterInput'
	],
	#category : #'Spec2-Core-Widgets-Table'
}

{ #category : #specs }
SpListWithFilterPresenter class >> defaultSpec [

	^ SpBoxLayout newVertical
		  add: #list;
		  add: #filterInput expand: false;
		  yourself
]

{ #category : #example }
SpListWithFilterPresenter class >> example [

	<sampleInstance>
	| example |
	example := self new.
	example list items: RBBrowserEnvironment default classes asArray.
	example openWithSpec.
	example withWindowDo: [ :window | 
		window title: self name asString , ' example' ].
	^ example
]

{ #category : #initialization }
SpListWithFilterPresenter >> connectPresenters [

	filterInput whenTextChangedDo: [ :text | self filterListItems: text ]
]

{ #category : #initialization }
SpListWithFilterPresenter >> enableItemBeginsWithFilter [

	itemFilter := [ :each :pattern | 
	              each asLowercase beginsWith: pattern asLowercase ]
]

{ #category : #initialization }
SpListWithFilterPresenter >> enableItemSubstringFilter [

	itemFilter := [ :each :pattern | 
	              each asLowercase includesSubstring: pattern asLowercase ]
]

{ #category : #accessing }
SpListWithFilterPresenter >> filterInput [
	^ filterInput
]

{ #category : #actions }
SpListWithFilterPresenter >> filterListItems: pattern [

	| filteredItems |
	unfilteredItems ifNil: [ unfilteredItems := list items ].
	pattern ifEmpty: [ 
		list items: unfilteredItems.
		^ self ].
	filteredItems := unfilteredItems select: [ :item | 
		                 itemFilter
			                 value: (list display value: item)
			                 value: pattern ].
	list items: filteredItems
]

{ #category : #initialization }
SpListWithFilterPresenter >> initializePresenters [

	filterInput := self newTextInput
		               placeholder: 'Filter...';
		               yourself.
	list := self newList.
	self enableItemSubstringFilter.
	self focusOrder
		add: list;
		add: filterInput
]

{ #category : #accessing }
SpListWithFilterPresenter >> itemFilter [

	^ itemFilter
]

{ #category : #api }
SpListWithFilterPresenter >> itemFilter: aBlock [

	itemFilter := aBlock
]

{ #category : #accessing }
SpListWithFilterPresenter >> list [
	^ list
]
