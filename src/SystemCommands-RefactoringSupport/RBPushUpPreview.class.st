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
	#name : #RBPushUpPreview,
	#superclass : #RBPushDownPreview,
	#instVars : [
		'label2',
		'classDropList'
	],
	#category : #'SystemCommands-RefactoringSupport'
}

{ #category : #specs }
RBPushUpPreview class >> defaultSpec [
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
RBPushUpPreview class >> example [
	<script>
	^ (self on: RBPushUpPreview withSelectors: Array new)
]

{ #category : #specs }
RBPushUpPreview class >> title [

	^ 'Pull methods up to'
]

{ #category : #'instance creation' }
RBPushUpPreview >> accept [
	refactoring superClass: self activeClass .
	refactoring selectors: (selectedMethods collect: #selector).
]

{ #category : #updating }
RBPushUpPreview >> activeClass [
	^classDropList selectedItem
]

{ #category : #initialization }
RBPushUpPreview >> classes [
	^ class allSuperclasses removeAllSuchThat: [ :each | each == Object or: [ each == ProtoObject ] ].
]

{ #category : #initialization }
RBPushUpPreview >> initializeDropList [
	classDropList := self newDropList.
	classDropList
		display: [ :scope | scope name ];
		iconBlock: [ :e | e systemIcon ]
]

{ #category : #initialization }
RBPushUpPreview >> initializeWidgets [
	super initializeWidgets .
	label2 := self newLabel.
	self initializeDropList.
]

{ #category : #initialization }
RBPushUpPreview >> on: aClass withSelectors: selectors [
	|classes|
	super on: aClass withSelectors: selectors.
	classes := self classes.
	classDropList items: classes.
	
	label2 label: self textLabel2
]

{ #category : #'acessing text' }
RBPushUpPreview >> textLabel [
	^ 'Methods to be pulled up'
]

{ #category : #'acessing text' }
RBPushUpPreview >> textLabel2 [ 
	^ 'Pull up methods of ', class name,' to:' .
]
