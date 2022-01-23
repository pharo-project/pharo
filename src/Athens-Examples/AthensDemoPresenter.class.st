"
I am a simple presenter to display the Athens demo.

I'll open an Athens demo morph and will display two buttons to navigate the different demos.

Examples
-------------

	self open
"
Class {
	#name : #AthensDemoPresenter,
	#superclass : #SpPresenter,
	#instVars : [
		'backButton',
		'forwardButton',
		'demo'
	],
	#category : #'Athens-Examples-Spec'
}

{ #category : #layout }
AthensDemoPresenter class >> defaultLayout [
	^ SpBoxLayout newTopToBottom
		add: #demo;
		add:
			(SpBoxLayout newLeftToRight
				add: #backButton;
				add: #forwardButton;
				yourself)
			withConstraints: [ :constraints | constraints height: self toolbarHeight ];
		yourself
]

{ #category : #'instance creation' }
AthensDemoPresenter class >> open [
	<script>
	self new open
]

{ #category : #initialization }
AthensDemoPresenter >> initializeWidgets [
	backButton := self newButton.
	forwardButton := self newButton.
	demo := self newMorph.

	demo morph: AthensDemoMorph new.

	backButton
		label: 'Previous Figure';
		action: [ demo morph prevFigure ].
	forwardButton
		label: 'Next Figure';
		action: [ demo morph nextFigure ].

	self focusOrder
		add: backButton;
		add: forwardButton
]

{ #category : #initialization }
AthensDemoPresenter >> initializeWindow: aWindowPresenter [
	aWindowPresenter
		title: 'Athens demos';
		initialExtent: demo morph extent + (0 @ 100)
]
