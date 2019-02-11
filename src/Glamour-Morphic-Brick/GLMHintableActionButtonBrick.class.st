Class {
	#name : #GLMHintableActionButtonBrick,
	#superclass : #GLMActionButtonBrick,
	#instVars : [
		'hintTitle',
		'hintText',
		'hintDelay',
		'hintModifier',
		'hintBrick'
	],
	#category : #'Glamour-Morphic-Brick-Widgets-UI'
}

{ #category : #initialization }
GLMHintableActionButtonBrick >> beError [

	self beMode: #beError
]

{ #category : #initialization }
GLMHintableActionButtonBrick >> beHelp [

	self beMode: #beHelp
]

{ #category : #initialization }
GLMHintableActionButtonBrick >> beMode: aMode [

	self hoverAction: [ :aBrick :evt | | delay |
		
		delay := (self isModifierPressed: evt) 
			ifTrue: [ 0 ]
			ifFalse: [ self hintDelay ].
		
		self startStepping: #openHintPopup: 
			at: Time millisecondClockValue + delay
			arguments: {aMode}
			stepTime: 3000 
	]
]

{ #category : #initialization }
GLMHintableActionButtonBrick >> beNormal [

	self beMode: #beNormal
]

{ #category : #initialization }
GLMHintableActionButtonBrick >> beSuccess [

	self beMode: #beSuccess
]

{ #category : #'brick-interactable-actions' }
GLMHintableActionButtonBrick >> click: evt [

	|result|
	
	result := super click: evt.
	result ifTrue: [ self closePopup ].
	
	^ result
]

{ #category : #'brick-interactable-actions' }
GLMHintableActionButtonBrick >> closePopup [

	self stopSteppingSelector: #openHintPopup:.

	hintBrick ifNotNil: [ 	
		hintBrick close.
		hintBrick := nil.
	]
]

{ #category : #accessing }
GLMHintableActionButtonBrick >> hintDelay [

	^ hintDelay ifNil: [ hintDelay := 700 ]
]

{ #category : #accessing }
GLMHintableActionButtonBrick >> hintDelay: anObject [
	hintDelay := anObject
]

{ #category : #accessing }
GLMHintableActionButtonBrick >> hintModifier [

	^ hintModifier ifNil: [ hintModifier := #shift ]
]

{ #category : #accessing }
GLMHintableActionButtonBrick >> hintModifier: anObject [

	hintModifier := anObject
]

{ #category : #accessing }
GLMHintableActionButtonBrick >> hintText [

	^ hintText ifNil: [ hintText := 'Need help?' ]
]

{ #category : #accessing }
GLMHintableActionButtonBrick >> hintText: anObject [

	hintText := anObject
]

{ #category : #accessing }
GLMHintableActionButtonBrick >> hintTitle [

	^ hintTitle ifNil: [ hintTitle := 'Hi,' ]
]

{ #category : #accessing }
GLMHintableActionButtonBrick >> hintTitle: anObject [
	hintTitle := anObject
]

{ #category : #initialization }
GLMHintableActionButtonBrick >> initialize [
	super initialize.
	
	self beHelp.
	self unhoverAction: [ :aBrick :anEvent |
		(aBrick globalBounds containsPoint: anEvent position) ifFalse: [ 
			self closePopup 
		] 
	]
]

{ #category : #testing }
GLMHintableActionButtonBrick >> isModifierPressed: anEvent [

	self hintModifier ifNil: [ ^ false ].
	self hintModifier = #shift ifTrue: [ ^ anEvent shiftPressed ].
	self hintModifier = #option ifTrue: [ ^ anEvent optionKeyPressed ].
	self hintModifier = #alt ifTrue: [ ^ anEvent altKeyPressed ].
	self hintModifier = #command ifTrue: [ ^ anEvent commandKeyPressed ].
	self hintModifier = #control ifTrue: [ ^ anEvent controlKeyPressed ].
	^ false
]

{ #category : #'brick-interactable-actions' }
GLMHintableActionButtonBrick >> openHintPopup: aMode [
	
	self closePopup.
	
	hintBrick := GLMPopupBrick new
		beUpward;
		hintText: self hintText;
		titleText: self hintTitle;
		perform: aMode withEnoughArguments: {  };
		openOn: self.
]

{ #category : #'brick-interactable-actions' }
GLMHintableActionButtonBrick >> press [
	super press.
	
	self closePopup.
]
