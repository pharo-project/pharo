"
A SpListWithFilterPresenterTest is a test class for testing the behavior of SpListWithFilterPresenter
"
Class {
	#name : #SpListWithFilterPresenterTest,
	#superclass : #TestCase,
	#instVars : [
		'listWithFilter'
	],
	#category : #'Spec2-Tests-Core-Widgets'
}

{ #category : #running }
SpListWithFilterPresenterTest >> setUp [

	super setUp.

	listWithFilter := SpListWithFilterPresenter new
]

{ #category : #test }
SpListWithFilterPresenterTest >> testEnableItemBeginsWithFilter [

	listWithFilter enableItemBeginsWithFilter.
	listWithFilter list items: { 
			Number.
			Character.
			String.
			OrderedCollection.
			HashedCollection.
			Array }.
	listWithFilter filterListItems: 'NUM'.
	self assert: listWithFilter list items size equals: 1.
	listWithFilter filterListItems: 'COLLECTION'.
	self assert: listWithFilter list items size equals: 0.
	listWithFilter filterListItems: 'ARACTER'.
	self assert: listWithFilter list items size equals: 0.
	listWithFilter filterListItems: 'STRIN'.
	self assert: listWithFilter list items size equals: 1.
	listWithFilter filterListItems: 'arrAY'.
	self assert: listWithFilter list items size equals: 1.
	listWithFilter filterListItems: ''.
	self assert: listWithFilter list items size equals: 6
]

{ #category : #test }
SpListWithFilterPresenterTest >> testEnableItemSubstringFilter [

	listWithFilter enableItemSubstringFilter.
	listWithFilter list items: { 
			Number.
			Character.
			String.
			OrderedCollection.
			HashedCollection.
			Array }.
	listWithFilter filterListItems: 'BER'.
	self assert: listWithFilter list items size equals: 1.
	listWithFilter filterListItems: 'COLLECTION'.
	self assert: listWithFilter list items size equals: 2.
	listWithFilter filterListItems: 'TRIN'.
	self assert: listWithFilter list items size equals: 1.
	listWithFilter filterListItems: 'arrAY'.
	self assert: listWithFilter list items size equals: 1.
	listWithFilter filterListItems: ''.
	self assert: listWithFilter list items size equals: 6
]

{ #category : #test }
SpListWithFilterPresenterTest >> testFilterListItems [

	| listItems |
	listItems := { 
		             OrderedCollection.
		             Array.
		             SequenceableCollection.
		             Set.
		             Dictionary }.
	listWithFilter list items: listItems.
	listWithFilter filterListItems: 'collection'.
	self assertCollection: listWithFilter list items hasSameElements: { 
			OrderedCollection.
			SequenceableCollection }.
	listWithFilter filterListItems: 'xyz'.
	self assertEmpty: listWithFilter list items.
	listWithFilter filterListItems: 'array'.
	self
		assertCollection: listWithFilter list items
		hasSameElements: { Array }.
	listWithFilter filterListItems: ''.
	self
		assertCollection: listWithFilter list items
		hasSameElements: listItems
]

{ #category : #test }
SpListWithFilterPresenterTest >> testFilterListItemsWithDifferentDisplay [

	| listItems |
	listItems := { OrderedCollection.
		             Array.
		             SequenceableCollection.
		             Set.
		             Dictionary }.
	listWithFilter list items: listItems.
	listWithFilter list display: [ :each | each name asHex ].
	listWithFilter filterListItems: 'Collection' asHex.
	self assertCollection: listWithFilter list items hasSameElements: { 
			OrderedCollection.
			SequenceableCollection }.
	listWithFilter filterListItems: 'xyz'.
	self assertEmpty: listWithFilter list items.
	listWithFilter filterListItems: 'Array' asHex.
	self
		assertCollection: listWithFilter list items
		hasSameElements: { Array }.
	listWithFilter filterListItems: ''.
	self
		assertCollection: listWithFilter list items
		hasSameElements: listItems
]

{ #category : #test }
SpListWithFilterPresenterTest >> testInitializePresenters [

	self assert: (listWithFilter list isMemberOf: SpListPresenter).
	self assert:
		(listWithFilter filterInput isMemberOf: SpTextInputFieldPresenter).
	self assertCollection: listWithFilter focusOrder equals: { 
			listWithFilter list.
			listWithFilter filterInput } asOrderedCollection
]

{ #category : #test }
SpListWithFilterPresenterTest >> testItemFilter [

	| filterBlock |
	filterBlock := [ :item :pattern | 
	               item asLowercase =
		               pattern asLowercase ].
	listWithFilter itemFilter: filterBlock.
	self assert: listWithFilter itemFilter equals: filterBlock
]

{ #category : #test }
SpListWithFilterPresenterTest >> testItems [

	listWithFilter list items: { Array }.
	self assertCollection: listWithFilter list items hasSameElements: { Array }
]
