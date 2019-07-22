"
I am a preview of push down method refactoring, I can select the methods of class to push down
"
Class {
	#name : #RBPushDownPreview,
	#superclass : #SpPresenter,
	#instVars : [
		'table',
		'label',
		'class',
		'selectedMethods',
		'selectedAbstractMethods',
		'refactoring'
	],
	#category : #'Refactoring-Missing-Preview'
}

{ #category : #specs }
RBPushDownPreview class >> defaultSpec [
	^ SpBoxLayout newVertical
		add: #label withConstraints: [ :c | c height: self buttonHeight ];
		add:
			(SpBoxLayout newHorizontal
				add: #table;
				yourself);
		yourself
]

{ #category : #examples }
RBPushDownPreview class >> on: aRefactoring [
	|temp|
	temp := self new on: aRefactoring .
	^ temp openModalWithSpec
]

{ #category : #examples }
RBPushDownPreview class >> on: aClass withSelectors: selectors [
	|temp|
	temp := self new 
		on: aClass 
		withSelectors: selectors asOrderedCollection.
	^ temp openModalWithSpec
]

{ #category : #specs }
RBPushDownPreview class >> title [

	^ 'Pull members down'
]

{ #category : #subclassresponsibility }
RBPushDownPreview >> accept [
	refactoring selectors: (selectedMethods collect: #selector)
]

{ #category : #'instance creation' }
RBPushDownPreview >> accept: presenter [
	self accept.
	presenter
				beOk;
				close
]

{ #category : #initialization }
RBPushDownPreview >> initializeChangesTree [
	table addColumn:
		((CheckBoxTableColumn title: ' ' evaluated: [ :aClass | 
	selectedMethods includes: aClass ])
				onActivation: [ :aClass | selectedMethods add: aClass ];
				onDesactivation: [ :aClass | selectedMethods remove: aClass ];
				width: 20;
				yourself);
			addColumn:  (StringTableColumn title: 'Members' evaluated: #selector );
			beResizable.
]

{ #category : #'instance creation' }
RBPushDownPreview >> initializeDialogWindow: aModalPresenter [
	aModalPresenter
		addButton: 'Refactor' do: [ :presenter | self accept: presenter ];
		addButton: 'Cancel' do: [ :presenter | 
			presenter
				beCancel;
				close ]
]

{ #category : #initialization }
RBPushDownPreview >> initializeWidgets [
	table := self newTable.
	label := (self newLabel) .
	
	self initializeChangesTree.
	
]

{ #category : #accessing }
RBPushDownPreview >> methods [
	^ class methods
]

{ #category : #initialization }
RBPushDownPreview >> on: aRefactoring [
	refactoring := aRefactoring.
	self on: refactoring class1 realClass 
		withSelectors: refactoring selectors asOrderedCollection.

]

{ #category : #initialization }
RBPushDownPreview >> on: aClass withSelectors: selectors [
	class := aClass.
	label label: self textLabel.
	selectedMethods := selectors.
	
	table items: self methods
]

{ #category : #subclassresponsibility }
RBPushDownPreview >> textLabel [
	^ 'Push members from ', class name,' down' .
]
