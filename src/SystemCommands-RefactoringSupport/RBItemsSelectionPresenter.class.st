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
	#name : #RBItemsSelectionPresenter,
	#superclass : #SpPresenter,
	#instVars : [
		'table',
		'label',
		'selectedItems'
	],
	#category : #'SystemCommands-RefactoringSupport'
}

{ #category : #specs }
RBItemsSelectionPresenter class >> defaultSpec [
	^ SpBoxLayout newTopToBottom
		add: #label withConstraints: [ :c | c height: self buttonHeight ];
		add:
			(SpBoxLayout newLeftToRight
				add: #table;
				yourself);
		yourself
]

{ #category : #specs }
RBItemsSelectionPresenter class >> label: aString withItems: items selecting: selectedItems [
	^ self new 
		label: aString withItems: items selecting: selectedItems;
		openBlockedDialogWithSpec 
]

{ #category : #actions }
RBItemsSelectionPresenter >> accept [

	selectedItems := table selectedItems
]

{ #category : #actions }
RBItemsSelectionPresenter >> accept: presenter [
	self accept.
	presenter
				beOk;
				close
]

{ #category : #attributes }
RBItemsSelectionPresenter >> columnName [
	self shouldBeImplemented
]

{ #category : #attributes }
RBItemsSelectionPresenter >> columnSelector [ 
	self shouldBeImplemented 
]

{ #category : #accessing }
RBItemsSelectionPresenter >> components [ 
	^ { table . label }
]

{ #category : #initialization }
RBItemsSelectionPresenter >> connectPresenters [
	
	table bindKeyCombination: Character backspace asShortcut 
		toAction: [ table toggleSelection ].
	self components do: [ :each |
	each bindKeyCombination: Character cr asKeyCombination 
		toAction: [ :presenter | self accept: presenter] ]
]

{ #category : #initialization }
RBItemsSelectionPresenter >> initializeDialogWindow: aModalPresenter [
	aModalPresenter
		addButton: 'Refactor' do: [ :presenter | self accept: presenter ];
		addButton: 'Cancel' do: [ :presenter | 
			presenter
				beCancel;
				close ]        
]

{ #category : #initialization }
RBItemsSelectionPresenter >> initializeWidgets [
	table := self instantiate: SpFilteringSelectableListPresenter.
	table display: self columnSelector.
	label := self newLabel.
]

{ #category : #initialization }
RBItemsSelectionPresenter >> label: aString withItems: coll1 selecting: coll2 [
	label label: aString.
	
	table items: coll1;
		selectItems: coll2	
]

{ #category : #accessing }
RBItemsSelectionPresenter >> selectedItems [
	^ selectedItems 
]
