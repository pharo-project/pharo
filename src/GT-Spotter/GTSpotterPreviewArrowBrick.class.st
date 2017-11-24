Class {
	#name : #GTSpotterPreviewArrowBrick,
	#superclass : #GLMBrick,
	#instVars : [
		'triangleBrick',
		'selectedItemBrick',
		'spotterModel',
		'isTriangleVisible'
	],
	#category : #'GT-Spotter-UI'
}

{ #category : #actions }
GTSpotterPreviewArrowBrick >> hideTriangle [

	isTriangleVisible := false.
	self themer spotterThemer spotterPreviewArrowWidgetStyleFor: self triangleBrick.
	self changed
]

{ #category : #initialization }
GTSpotterPreviewArrowBrick >> initialize [
	super initialize.
	
	self triangleBrick: self newTriangleBrick
]

{ #category : #testing }
GTSpotterPreviewArrowBrick >> isTriangleVisible [

	^ isTriangleVisible ifNil: [ isTriangleVisible := false ]
]

{ #category : #actions }
GTSpotterPreviewArrowBrick >> moveTo: anItemBrick [
	|itemBounds delta selfBounds|
	anItemBrick ifNil: [ ^ self ].
	
	itemBounds := anItemBrick globalBounds.
	selfBounds := self globalBounds.
	
	delta := ((itemBounds top - selfBounds top) max: 0) min: (selfBounds height - self triangleBrick height).
	self triangleBrick brickBounds top = delta ifTrue: [ ^ self ].
	
	self triangleBrick marginTop: delta.
	self triangleBrick doLayoutForce.
	self owner changed
	
]

{ #category : #'instance creation' }
GTSpotterPreviewArrowBrick >> newTriangleBrick [

	^ GLMHintableActionButtonBrick new
]

{ #category : #'event handling' }
GTSpotterPreviewArrowBrick >> onAllCandidatesRemoved [

	self selectedItemBrick: nil
]

{ #category : #'event handling' }
GTSpotterPreviewArrowBrick >> onCandidateMoved [

	self moveTo: self selectedItemBrick
]

{ #category : #'event handling' }
GTSpotterPreviewArrowBrick >> onCandidateSelected: ann [

	self selectedItemBrick: ann itemBrick.
	self moveTo: self selectedItemBrick
]

{ #category : #'brick-drawing' }
GTSpotterPreviewArrowBrick >> onDraw: aCanvas [
	super onDraw: aCanvas.
	
		aCanvas
			fillRectangle: ((self brickBounds withWidth: self themer spotterThemer outerBorderWidth) translateBy: (self themer scrollbarWidth + self themer scrollbarOffset + 1)@0)
			color: (self spotterModel isPreviewVisible
				ifTrue: [ self themer spotterThemer backgroundColor ]
				ifFalse: [ Color transparent ])
	
	
]

{ #category : #'event handling' }
GTSpotterPreviewArrowBrick >> onHideHints [

	self themer spotterThemer hintHideArrowStyleFor: self triangleBrick
]

{ #category : #'event handling' }
GTSpotterPreviewArrowBrick >> onHidePreview [

	self triangleBrick renderer border: self themer spotterThemer outerBorderWidth.
	self triangleBrick selectedColor: Color transparent.
	self changed
]

{ #category : #'event handling' }
GTSpotterPreviewArrowBrick >> onRevealHints [

	self themer spotterThemer hintRevealArrowStyleFor: self triangleBrick
]

{ #category : #'event handling' }
GTSpotterPreviewArrowBrick >> onShowPreview [

	self triangleBrick renderer border: 0.
	self triangleBrick selectedColor: self themer spotterThemer borderColor.
	self changed
]

{ #category : #'event handling' }
GTSpotterPreviewArrowBrick >> onStepAdded [
	
	self selectedItemBrick: nil.
	self spotterModel steps last candidates announcer when: GTSpotterAllCandidatesRemoved send: #onAllCandidatesRemoved to: self.
]

{ #category : #'brick-properties-events' }
GTSpotterPreviewArrowBrick >> onThemerChanged [
	super onThemerChanged.
	
	self themer spotterThemer spotterPreviewArrowWidgetStyleFor: self triangleBrick
]

{ #category : #accessing }
GTSpotterPreviewArrowBrick >> selectedItemBrick [
	
	^ selectedItemBrick
]

{ #category : #accessing }
GTSpotterPreviewArrowBrick >> selectedItemBrick: aBrick [
	
	selectedItemBrick isNil & aBrick isNotNil ifTrue: [ self showTriangle ].
	selectedItemBrick isNotNil & aBrick isNil ifTrue: [ self hideTriangle ].
	
	selectedItemBrick := aBrick
]

{ #category : #actions }
GTSpotterPreviewArrowBrick >> showTriangle [

	isTriangleVisible := true.
	self themer spotterThemer spotterPreviewArrowWidgetStyleFor: self triangleBrick.
	self changed
]

{ #category : #accessing }
GTSpotterPreviewArrowBrick >> spotterModel [

	^ spotterModel
]

{ #category : #accessing }
GTSpotterPreviewArrowBrick >> spotterModel: aSpotterModel [

	spotterModel := aSpotterModel.
	
	self spotterModel announcer when: GTSpotterCandidateSelected send: #onCandidateSelected: to: self.
	self spotterModel announcer when: GTSpotterCandidateSelectedMoved send: #onCandidateMoved to: self.
	self spotterModel announcer when: GTSpotterStepAdded send: #onStepAdded to: self.
	self spotterModel announcer when: GTSpotterShowPreview send: #onShowPreview to: self.
	self spotterModel announcer when: GTSpotterHidePreview send: #onHidePreview to: self.
	
	self spotterModel announcer when: GTSpotterRevealHints send: #onRevealHints to: self.
	self spotterModel announcer when: GTSpotterHideHints send: #onHideHints to: self.
]

{ #category : #accessing }
GTSpotterPreviewArrowBrick >> triangleBrick [

	^ triangleBrick
]

{ #category : #accessing }
GTSpotterPreviewArrowBrick >> triangleBrick: aBrick [

	triangleBrick := aBrick.
	self triangleBrick
		hintTitle: 'Toggle preview';
		hintText: '<',OSPlatform current defaultModifier name asLowercase,'+p>';
		action: [ self spotterModel togglePreview ].
	self themer spotterThemer spotterPreviewArrowStyleFor: self triangleBrick.
	self addBrickBack: self triangleBrick
	
]
