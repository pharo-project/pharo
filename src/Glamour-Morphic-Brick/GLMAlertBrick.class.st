Class {
	#name : #GLMAlertBrick,
	#superclass : #GLMBandBrick,
	#instVars : [
		'textBrick',
		'okButton'
	],
	#category : #Glamour-Morphic-Brick-Widgets-UI
}

{ #category : #open }
GLMAlertBrick class >> alert: anObject [

	self new alert: anObject
	
]

{ #category : #examples }
GLMAlertBrick class >> exampleOpen [

	'Hello world!' brickAlert
]

{ #category : #examples }
GLMAlertBrick class >> gtExample [
	<gtExample>
	<label: 'Simple alert window displaying a message'>
	
	^ self new alertOn: 'Hello world!'
	
]

{ #category : #scripting }
GLMAlertBrick >> alert: anObject [
	|window|
	self alertOn: anObject.
	window := (self openInBrickWindowLabeled: 'Alert').
	self class environment at: #GLMSystemWindowBrick ifPresent: [ 
		window
			noMenu;
			innerExtent: self extent ].

]

{ #category : #scripting }
GLMAlertBrick >> alertOn: anObject [

	self textBrick
		text: anObject asString
]

{ #category : #initialization }
GLMAlertBrick >> initialize [
	super initialize.
	
	self
		width:  300;
		textBrick: self newTextBrick;
		okButton: self newButtonBrick
	
]

{ #category : #'instance creation' }
GLMAlertBrick >> newButtonBrick [

	^ GLMButtonBrick new
]

{ #category : #'instance creation' }
GLMAlertBrick >> newTextBrick [

	^ GLMRubEditingAreaBrick new
]

{ #category : #accessing }
GLMAlertBrick >> okButton [
	^ okButton
]

{ #category : #accessing }
GLMAlertBrick >> okButton: anObject [
	okButton := anObject.
	
	self okButton
		marginBottom: 3;
		text: 'OK';
		hAlign: #center;
		when: #onClicked do: [ self window ifNotNil: [ :w | w close ] ].
	
	self addBrickBack: self okButton
]

{ #category : #accessing }
GLMAlertBrick >> textBrick [

	^ textBrick
]

{ #category : #accessing }
GLMAlertBrick >> textBrick: aBrick [

	textBrick := aBrick.
	
	self textBrick
		margin: #(0 3 3 3);
		vShrinkWrap.
	
	self addBrickBack: self textBrick
]
