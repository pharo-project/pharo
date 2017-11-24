Class {
	#name : #GTSpotterItemActionbarBrick,
	#superclass : #GLMBrick,
	#instVars : [
		'stepModel',
		'diveInButton',
		'actOnButton',
		'candidateLink'
	],
	#category : #'GT-Spotter-UI'
}

{ #category : #accessing }
GTSpotterItemActionbarBrick >> actOnButton [
	^ actOnButton
]

{ #category : #adding }
GTSpotterItemActionbarBrick >> addActActionButton [

	actOnButton := (self newActionButtonBrick
			icon: self themer spotterThemer iconAct;
			hintTitle: 'Default action';
			hintText: '<enter>';
			action: [ self owner beSelected. self stepModel actDueTo: #actionButton ];
			yourself).

	self addActionButton: actOnButton.
]

{ #category : #adding }
GTSpotterItemActionbarBrick >> addActionButton: anActionButton [

	self themer spotterThemer itemActionButtonStyleFor: anActionButton.
	self themer spotterThemer itemActionButtonWidgetStyleFor: anActionButton.
		
	self addBrickBack: anActionButton.

	self stepModel spotter isShiftPressed
		ifTrue: [ self themer spotterThemer hintRevealStyleFor: anActionButton ].
]

{ #category : #adding }
GTSpotterItemActionbarBrick >> addDiveInActionButton [
			
	(self stepModel canDiveIn: self candidate) ifTrue: [
		diveInButton := (self newActionButtonBrick
			icon: self themer spotterThemer iconDiveIn;
			hintTitle: 'Dive in';
			hintText: '<',OSPlatform current defaultModifier name asLowercase,'+right>';
			action: [ self owner beSelected. self stepModel diveInDueTo: #actionButton ];
			yourself).
		
		self addActionButton: diveInButton ]
]

{ #category : #accessing }
GTSpotterItemActionbarBrick >> candidate [

	^ self candidateLink value
]

{ #category : #accessing }
GTSpotterItemActionbarBrick >> candidateLink [
	^ candidateLink
]

{ #category : #accessing }
GTSpotterItemActionbarBrick >> candidateLink: anObject [
	candidateLink := anObject
]

{ #category : #accessing }
GTSpotterItemActionbarBrick >> diveInButton [
	^ diveInButton
]

{ #category : #testing }
GTSpotterItemActionbarBrick >> hasStepModel [
	^ stepModel isNotNil
]

{ #category : #'instance creation' }
GTSpotterItemActionbarBrick >> newActionButtonBrick [

	^ GLMHintableActionButtonBrick new
]

{ #category : #'event handling' }
GTSpotterItemActionbarBrick >> onHideHints [

	self visible ifFalse: [ ^ self ].
	self themer spotterThemer hintHideStyleFor: self diveInButton.
	self themer spotterThemer hintHideStyleFor: self actOnButton.
]

{ #category : #'event handling' }
GTSpotterItemActionbarBrick >> onRevealHints [

	self visible ifFalse: [ ^ self ].
	self themer spotterThemer hintRevealStyleFor: self diveInButton.
	self themer spotterThemer hintRevealStyleFor: self actOnButton
]

{ #category : #'brick-properties-events' }
GTSpotterItemActionbarBrick >> onThemerChanged [
	super onThemerChanged.
	
	self themer spotterThemer itemActionbarStyleFor: self.
	self subbricks do: [ :aBrick | self themer spotterThemer itemActionButtonWidgetStyleFor: aBrick ]
]

{ #category : #accessing }
GTSpotterItemActionbarBrick >> stepModel [
	^ stepModel
]

{ #category : #accessing }
GTSpotterItemActionbarBrick >> stepModel: anObject [
	stepModel := anObject.
	
	self stepModel spotter announcer when: GTSpotterRevealHints send: #onRevealHints to: self.
	self stepModel spotter announcer when: GTSpotterHideHints send: #onHideHints to: self.
	
	self
		addActActionButton;
		addDiveInActionButton
]

{ #category : #actions }
GTSpotterItemActionbarBrick >> toggleHints [

	self visible ifFalse: [ ^ self ].
	self stepModel spotter isShiftPressed
		ifTrue: [ self onRevealHints ]
		ifFalse: [ self onHideHints ]
]

{ #category : #actions }
GTSpotterItemActionbarBrick >> visible: aBoolean [
	super visible: aBoolean.
	
	self toggleHints
	
	
]
