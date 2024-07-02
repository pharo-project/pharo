"
I am an abstract class to select items from a list. 
My subclasses should implement 3 methods:

	- Class side
		- title
	- Instance side
		- columnName
		- columnSelector

Instance Variables
-------------------------------------------------

table:                   <TablePresenter> A table to display the possible methods to push down
label:                   <LabelPresenter> Title of table
items:                   <OrderedCollection> A list to select 
selectedItems:           <OrderedCollection> A list with selected items
"
Class {
	#name : 'StItemsSelectionPresenter',
	#superclass : 'SpPresenter',
	#instVars : [
		'table',
		'label',
		'selectedItems'
	],
	#category : 'Refactoring-UI-UI',
	#package : 'Refactoring-UI',
	#tag : 'UI'
}

{ #category : 'layout' }
StItemsSelectionPresenter class >> defaultLayout [
	^ SpBoxLayout newTopToBottom
		add: #label withConstraints: [ :c | c height: self buttonHeight ];
		add:
			(SpBoxLayout newLeftToRight
				add: #table;
				yourself);
		yourself
]

{ #category : 'specs' }
StItemsSelectionPresenter class >> label: aString withItems: items selecting: selectedItems [
	^ self new
		label: aString withItems: items selecting: selectedItems;
		openBlockedDialog;
		yourself 
]

{ #category : 'actions' }
StItemsSelectionPresenter >> accept [

	selectedItems := table selectedItems
]

{ #category : 'actions' }
StItemsSelectionPresenter >> accept: presenter [
	self accept.
	presenter
				beOk;
				close
]

{ #category : 'TOREMOVE' }
StItemsSelectionPresenter >> cancelled [ 

	^ self window cancelled 
]

{ #category : 'attributes' }
StItemsSelectionPresenter >> columnName [
	self shouldBeImplemented
]

{ #category : 'attributes' }
StItemsSelectionPresenter >> columnSelector [
	self shouldBeImplemented
]

{ #category : 'accessing' }
StItemsSelectionPresenter >> components [
	^ { table . label }
]

{ #category : 'initialization' }
StItemsSelectionPresenter >> connectPresenters [

	table bindKeyCombination: Character backspace asShortcut
		toAction: [ table toggleSelection ].
	self components do: [ :each |
	each bindKeyCombination: Character cr asKeyCombination
		toAction: [ :presenter | self accept: presenter] ]
]

{ #category : 'initialization' }
StItemsSelectionPresenter >> initializeDialogWindow: aModalPresenter [
	
	aModalPresenter
		addButton: 'Cancel' do: [ :presenter | presenter beCancel; close ];
		addDefaultButton: 'Refactor' do: [ :presenter | self accept: presenter ]
]

{ #category : 'initialization - deprecated' }
StItemsSelectionPresenter >> initializeWidgets [
	table := self instantiate: SpFilteringSelectableListPresenter.
	table display: self columnSelector.
	label := self newLabel
]

{ #category : 'initialization' }
StItemsSelectionPresenter >> label: aString withItems: coll1 selecting: coll2 [

	label label: aString.

	table items: coll1;
		selectItems: coll2
]

{ #category : 'accessing' }
StItemsSelectionPresenter >> selectedItems [
	^ selectedItems
]
