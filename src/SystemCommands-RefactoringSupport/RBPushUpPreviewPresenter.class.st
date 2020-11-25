"
I am a preview of Pull up refactoring, I can select the superClass of pull up methos and select the methods of class to pull up.


EXAMPLES
-------------------------------------------------

RBPushUpPreview on: RBPushDownPreview withSelectors: Array new.

Instance Variables
-------------------------------------------------

label:         <LabelPresenter> A label of droplist
label2:        <LabelPresenter> A  label of table
classDropList: <DropListPresenter> A drop list to select the superclass to push up the selected methods
"
Class {
	#name : #RBPushUpPreviewPresenter,
	#superclass : #RBPushDownPreviewPresenter,
	#instVars : [
		'label2',
		'classDropList'
	],
	#category : #'SystemCommands-RefactoringSupport'
}

{ #category : #specs }
RBPushUpPreviewPresenter class >> defaultSpec [
	^ SpBoxLayout newVertical
		add: #label2 withConstraints: [ :c | c height: self labelHeight ];
		add:
			(SpBoxLayout newHorizontal
				add: #classDropList ;
				yourself) withConstraints: [ :c | c height: self toolbarHeight  ];
		add: #label withConstraints: [ :c | c height: self buttonHeight ];
		add:
			(SpBoxLayout newHorizontal
				add: #table;
				yourself);
		yourself
]

{ #category : #examples }
RBPushUpPreviewPresenter class >> example [
	<script>
	^ (self on: RBPushUpPreviewPresenter withSelectors: Array new)
]

{ #category : #specs }
RBPushUpPreviewPresenter class >> title [

	^ 'Pull methods up to'
]

{ #category : #'instance creation' }
RBPushUpPreviewPresenter >> accept [
	refactoring superClass: self activeClass .
	refactoring selectors: (selectedMethods collect: #selector).
]

{ #category : #updating }
RBPushUpPreviewPresenter >> activeClass [
	^classDropList selectedItem
]

{ #category : #initialization }
RBPushUpPreviewPresenter >> classes [
	^ class allSuperclasses removeAllSuchThat: [ :each | each == Object or: [ each == ProtoObject ] ].
]

{ #category : #initialization }
RBPushUpPreviewPresenter >> initializeDropList [
	classDropList := self newDropList.
	classDropList
		display: [ :scope | scope name ];
		iconBlock: [ :e | e systemIcon ]
]

{ #category : #initialization }
RBPushUpPreviewPresenter >> initializeWidgets [
	super initializeWidgets .
	label2 := self newLabel.
	self initializeDropList.
]

{ #category : #initialization }
RBPushUpPreviewPresenter >> on: aClass withSelectors: selectors [
	|classes|
	super on: aClass withSelectors: selectors.
	classes := self classes.
	classDropList items: classes.
	
	label2 label: self textLabel2
]

{ #category : #'acessing text' }
RBPushUpPreviewPresenter >> textLabel [
	^ 'Methods to be pulled up'
]

{ #category : #'acessing text' }
RBPushUpPreviewPresenter >> textLabel2 [ 
	^ 'Pull up methods of ', class name,' to:' .
]
