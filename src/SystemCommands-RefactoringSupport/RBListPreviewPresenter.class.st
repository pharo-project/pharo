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
	#name : #RBListPreviewPresenter,
	#superclass : #SpPresenter,
	#instVars : [
		'table',
		'label',
		'items',
		'selectedItems'
	],
	#category : #'SystemCommands-RefactoringSupport'
}

{ #category : #visiting }
RBListPreviewPresenter class >> defaultSpec [
	^ SpBoxLayout newVertical
		add: #label withConstraints: [ :c | c height: self buttonHeight ];
		add:
			(SpBoxLayout newHorizontal
				add: #table;
				yourself);
		yourself
]

{ #category : #visiting }
RBListPreviewPresenter class >> label: aString withItems: coll1 selecting: coll2 [
	^ self new 
		label: aString withItems: coll1 selecting: coll2;
		openModalWithSpec 
]

{ #category : #actions }
RBListPreviewPresenter >> accept [
	"refactoring selectors: (selectedMethods collect: #selector)"
]

{ #category : #actions }
RBListPreviewPresenter >> accept: presenter [
	self accept.
	presenter
				beOk;
				close
]

{ #category : #attributes }
RBListPreviewPresenter >> columnName [
	self shouldBeImplemented
]

{ #category : #attributes }
RBListPreviewPresenter >> columnSelector [ 
	self shouldBeImplemented 
]

{ #category : #initialization }
RBListPreviewPresenter >> initializeChangesTree [
	table addColumn:
		((SpCheckBoxTableColumn title: ' ' evaluated: [ :aClass | 
	selectedItems includes: aClass ])
				onActivation: [ :aClass | selectedItems add: aClass ];
				onDeactivation: [ :aClass | selectedItems remove: aClass ];
				width: 20;
				yourself);
			addColumn: (SpStringTableColumn 
				title: self columnName 
				evaluated: self columnSelector );
			beResizable.
]

{ #category : #initialization }
RBListPreviewPresenter >> initializeDialogWindow: aModalPresenter [
	aModalPresenter
		addButton: 'Refactor' do: [ :presenter | self accept: presenter ];
		addButton: 'Cancel' do: [ :presenter | 
			presenter
				beCancel;
				close ]        
]

{ #category : #initialization }
RBListPreviewPresenter >> initializeWidgets [
	table := self newTable.
	label := self newLabel.
	
	self initializeChangesTree.
]

{ #category : #initialization }
RBListPreviewPresenter >> label: aString withItems: coll1 selecting: coll2 [
	label label: aString.
	selectedItems := coll2.
	
	table items: coll1
]

{ #category : #accessing }
RBListPreviewPresenter >> selectedItems [
	^ selectedItems 
]
