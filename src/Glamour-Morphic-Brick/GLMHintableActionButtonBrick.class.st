Class {
	#name : #GLMHintableActionButtonBrick,
	#superclass : #GLMActionButtonBrick,
	#instVars : [
		'hintTitle',
		'hintText',
		'hintDelay',
		'hintModifier'
	],
	#classInstVars : [
		'asyncTask',
		'hintBrick'
	],
	#category : #'Glamour-Morphic-Brick-Widgets-UI'
}

{ #category : #converting }
GLMHintableActionButtonBrick class >> asyncTaskUniqueInstance [

	^ asyncTask ifNil: [ asyncTask := GLMAsyncTask new ]
]

{ #category : #cleanup }
GLMHintableActionButtonBrick class >> cleanUp [

	self closeHint.
	self reset.
]

{ #category : #actions }
GLMHintableActionButtonBrick class >> closeHint [

	self asyncTaskUniqueInstance ifNotNil: #terminate.
	self hintBrickUniqueInstance ifNotNil: #close
]

{ #category : #accessing }
GLMHintableActionButtonBrick class >> hintBrickUniqueInstance [

	^ hintBrick
]

{ #category : #actions }
GLMHintableActionButtonBrick class >> openHint: aHintString title: aTitleString type: aTypeSymbol for: aBrick [
	|brick|
	
	self closeHint.
	brick :=  GLMPopupBrick new
		beUpward;
		hintText: aHintString;
		titleText: aTitleString;
		perform: aTypeSymbol withEnoughArguments: {  };
		openOn: aBrick.
		
	hintBrick := brick
		
	
]

{ #category : #actions }
GLMHintableActionButtonBrick class >> reset [

	self closeHint.
	asyncTask := nil.
	hintBrick := nil.
]

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

	self hoverAction: [ :aBrick :evt |
			self class asyncTaskUniqueInstance
				perform: [ self isInWorld ifTrue: [ self class openHint: self hintText title: self hintTitle type: aMode for: self ] ]
				delay: ((self isModifierPressed: evt) ifTrue: [ 0 ] ifFalse: [ self hintDelay ]) ]
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
	self class asyncTaskUniqueInstance terminate.
	self class reset.
	^ result
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
		self class asyncTaskUniqueInstance terminate.
		(aBrick globalBounds containsPoint: anEvent position)
			ifFalse: [ self class 
				closeHint;
				reset ] ]
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
GLMHintableActionButtonBrick >> press [
	super press.
	
	self class asyncTaskUniqueInstance terminate.
	self class reset.
]
