"
I am a preview of Pull up refactoring, I can select the superClass of pull up methos, and select the methods of class to pull up
"
Class {
	#name : #RBPullUpPreview,
	#superclass : #RBPushDownPreview,
	#instVars : [
		'label2',
		'classDropList'
	],
	#category : #'SystemCommands-RefactoringSupport'
}

{ #category : #specs }
RBPullUpPreview class >> defaultSpec [
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
RBPullUpPreview class >> example [
	<script>
	^ (self on: RBPullUpPreview withSelectors: Array new)
]

{ #category : #specs }
RBPullUpPreview class >> title [

	^ 'Pull methods up to'
]

{ #category : #'instance creation' }
RBPullUpPreview >> accept [
	refactoring superClass: self activeClass .
	refactoring selectors: (selectedMethods collect: #selector).
]

{ #category : #updating }
RBPullUpPreview >> activeClass [
	^classDropList selectedItem
]

{ #category : #initialization }
RBPullUpPreview >> classes [
	^ class allSuperclasses removeAllSuchThat: [ :each | each == Object or: [ each == ProtoObject ] ].
]

{ #category : #initialization }
RBPullUpPreview >> initializeDropList [
	classDropList := self newDropList.
	classDropList displayBlock: [ :scope | scope name ];
		iconBlock: [ :e | e systemIcon ]
]

{ #category : #initialization }
RBPullUpPreview >> initializeWidgets [
	super initializeWidgets .
	label2 := self newLabel.
	self initializeDropList.
]

{ #category : #initialization }
RBPullUpPreview >> on: aClass withSelectors: selectors [
	|classes|
	super on: aClass withSelectors: selectors.
	classes := self classes.
	classDropList items: classes.
	
	label2 label: self textLabel2
]

{ #category : #'acessing text' }
RBPullUpPreview >> textLabel [
	^ 'Methods to be pulled up'
]

{ #category : #'acessing text' }
RBPullUpPreview >> textLabel2 [ 
	^ 'Pull up methods of ', class name,' to:' .
]
