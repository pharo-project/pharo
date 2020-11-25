"
I am a preview of push down method refactoring. I can select the methods of class to push down

EXAMPLES
-------------------------------------------------

RBPushDownPreview on: RBPushDownPreview withSelectors: Array new.

Instance Variables
-------------------------------------------------

table:                   <TablePresenter> A table to display the possible methods to push down
label:                   <LabelPresenter> Title of table
class:                   <Class> class of selected methods
selectedMethods:         <OrderedCollection> A list with selected methods
refactoring:             <RBRefactoring> A subclass of RBRefactoring (RBPushUpRefactoring, RBPushDownRefactoring) to update according to the view modifications
"
Class {
	#name : #RBPushDownPreviewPresenter,
	#superclass : #SpPresenter,
	#instVars : [
		'table',
		'label',
		'class',
		'selectedMethods',
		'refactoring'
	],
	#category : #'SystemCommands-RefactoringSupport'
}

{ #category : #specs }
RBPushDownPreviewPresenter class >> defaultSpec [
	^ SpBoxLayout newVertical
		add: #label withConstraints: [ :c | c height: self buttonHeight ];
		add:
			(SpBoxLayout newHorizontal
				add: #table;
				yourself);
		yourself
]

{ #category : #examples }
RBPushDownPreviewPresenter class >> example [
	<script>
	^ (self on: RBPushUpPreviewPresenter withSelectors: Array new)
]

{ #category : #examples }
RBPushDownPreviewPresenter class >> on: aRefactoring [
	^ self new on: aRefactoring;
		openModalWithSpec
]

{ #category : #examples }
RBPushDownPreviewPresenter class >> on: aClass withSelectors: selectors [
	^ self new 
		on: aClass 
		withSelectors: selectors asOrderedCollection;
		openModalWithSpec
]

{ #category : #specs }
RBPushDownPreviewPresenter class >> title [

	^ 'Pull methods down'
]

{ #category : #subclassresponsibility }
RBPushDownPreviewPresenter >> accept [
	refactoring selectors: (selectedMethods collect: #selector)
]

{ #category : #'instance creation' }
RBPushDownPreviewPresenter >> accept: presenter [
	self accept.
	presenter
				beOk;
				close
]

{ #category : #initialization }
RBPushDownPreviewPresenter >> initializeChangesTree [
	table addColumn:
		((SpCheckBoxTableColumn title: ' ' evaluated: [ :aClass | 
	selectedMethods includes: aClass ])
				onActivation: [ :aClass | selectedMethods add: aClass ];
				onDeactivation: [ :aClass | selectedMethods remove: aClass ];
				width: 20;
				yourself);
			addColumn:  (SpStringTableColumn title: 'Methods' evaluated: #selector );
			beResizable.
]

{ #category : #'instance creation' }
RBPushDownPreviewPresenter >> initializeDialogWindow: aModalPresenter [
	aModalPresenter
		addButton: 'Refactor' do: [ :presenter | self accept: presenter ];
		addButton: 'Cancel' do: [ :presenter | 
			presenter
				beCancel;
				close ]
]

{ #category : #initialization }
RBPushDownPreviewPresenter >> initializeWidgets [
	table := self newTable.
	label := self newLabel.
	
	self initializeChangesTree.
	
]

{ #category : #accessing }
RBPushDownPreviewPresenter >> methods [
	^ class methods sort: [ :a :b | a asString < b asString ]
]

{ #category : #initialization }
RBPushDownPreviewPresenter >> on: aRefactoring [
	refactoring := aRefactoring.
	self on: refactoring targetClass realClass 
		withSelectors: refactoring selectors asOrderedCollection.

]

{ #category : #initialization }
RBPushDownPreviewPresenter >> on: aClass withSelectors: selectors [
	class := aClass.
	label label: self textLabel.
	selectedMethods := selectors.
	
	table items: self methods
]

{ #category : #subclassresponsibility }
RBPushDownPreviewPresenter >> textLabel [
	^ 'Push methods from ', class name,' down' .
]
