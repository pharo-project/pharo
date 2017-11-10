"
A RadioButtonGroupExample is an simple example about how to use RadioButtonGroupPresenter.

self example
"
Class {
	#name : #RadioButtonGroupExample,
	#superclass : #ComposablePresenter,
	#instVars : [
		'button1',
		'button2',
		'button3',
		'container',
		'morph1',
		'morph2',
		'morph3'
	],
	#category : #Spec-Examples-Morphic
}

{ #category : #specs }
RadioButtonGroupExample class >> defaultSpec [
	<spec: #default>
	
	^ SpecLayout composed
		newRow: [ :row |
			row 
				add: #button1;
				add: #button2;
				add: #button3 ] height: self toolbarHeight;
		add: #container;
		yourself
]

{ #category : #example }
RadioButtonGroupExample class >> example [
	^ self new openWithSpec
]

{ #category : #specs }
RadioButtonGroupExample class >> title [

	^ 'Radio Button Group Example'
]

{ #category : #accessing }
RadioButtonGroupExample >> button1 [

	^ button1
]

{ #category : #accessing }
RadioButtonGroupExample >> button2 [

	^ button2
]

{ #category : #accessing }
RadioButtonGroupExample >> button3 [

	^ button3
]

{ #category : #accessing }
RadioButtonGroupExample >> container [

	^ container asSpecAdapter
]

{ #category : #initialization }
RadioButtonGroupExample >> initialize [

	self initializeMorphs.
	super initialize.

	container := PanelMorph new.
	container
		changeTableLayout;
		listDirection: #bottomToLeft.
]

{ #category : #initialization }
RadioButtonGroupExample >> initializeMorphs [

	morph1 := Morph new
					color: Color red;
					width: 60;
					height: 20;
					yourself.
	morph2 := Morph new
					color: Color blue;
					width: 20;
					height: 60;
					yourself.
	morph3 := Morph new
					color: Color green;
					width: 50;
					height: 50;
					yourself.
]

{ #category : #initialization }
RadioButtonGroupExample >> initializePresenter [

	self on: button1 show: morph1.
	self on: button2 show: morph2.
	self on: button3 show: morph3.
	
]

{ #category : #initialization }
RadioButtonGroupExample >> initializeWidgets [
	button1 := self instantiate: RadioButtonPresenter.
	button2 := self instantiate: RadioButtonPresenter.
	button3 := self instantiate: RadioButtonPresenter.
	button1 label: 'Button 1'.
	button2 label: 'Button 2'.
	button3 label: 'Button 3'.
	self setFocus.
	self setGroup
]

{ #category : #initialization }
RadioButtonGroupExample >> on: button show: aMorph [

	button whenActivatedDo: [ container addMorph: aMorph ].
	button whenDeactivatedDo: [ container removeMorph: aMorph ]
]

{ #category : #initialization }
RadioButtonGroupExample >> setFocus [
	
	self focusOrder
		add: button1;
		add: button2;
		add: button3.
	
]

{ #category : #initialization }
RadioButtonGroupExample >> setGroup [
RadioButtonGroupPresenter new
		addRadioButton: button1;
		addRadioButton: button2;
		addRadioButton: button3;
		default: button1.
]
