Class {
	#name : #GTSpotterPaneBrick,
	#superclass : #GLMBrick,
	#instVars : [
		'contentsBrick',
		'previewBrick',
		'arrowBrick'
	],
	#category : #'GT-Spotter-UI'
}

{ #category : #accessing }
GTSpotterPaneBrick >> arrowBrick [

	^ arrowBrick
]

{ #category : #accessing }
GTSpotterPaneBrick >> arrowBrick: aBrick [

	arrowBrick := aBrick.
	self themer spotterThemer spotterPreviewArrowContainerStyleFor: self arrowBrick.
	self addBrickBack: self arrowBrick
]

{ #category : #accessing }
GTSpotterPaneBrick >> contentsBrick [

	^ contentsBrick
]

{ #category : #accessing }
GTSpotterPaneBrick >> contentsBrick: aBrick [
	
	contentsBrick ifNotNil: [self removeBrick: contentsBrick].
	contentsBrick := aBrick.
		
	self themer spotterThemer spotterContentsStyleFor: contentsBrick.

	self addBrickBack: contentsBrick
]

{ #category : #action }
GTSpotterPaneBrick >> hidePreview [

	self isPreviewShown ifFalse: [ ^ self ].

	self removeBrick: self previewBrick.
	self contentsBrick layout hSpaceFill.
	self changed.
]

{ #category : #initialization }
GTSpotterPaneBrick >> initialize [
	super initialize.
	
	self contentsBrick: self newContentsBrick.
	self arrowBrick: self newArrowBrick.
	self previewBrick: self newPreviewBrick
]

{ #category : #testing }
GTSpotterPaneBrick >> isPreviewShown [

	^ self hasSubbrick: self previewBrick
]

{ #category : #'instance creation' }
GTSpotterPaneBrick >> newArrowBrick [

	^ GTSpotterPreviewArrowBrick new
]

{ #category : #'instance creation' }
GTSpotterPaneBrick >> newContentsBrick [

	^ GTSpotterContentsBrick new
]

{ #category : #'instance creation' }
GTSpotterPaneBrick >> newPreviewBrick [

	^ GTSpotterPreviewBrick new
]

{ #category : #accessing }
GTSpotterPaneBrick >> previewBrick [

	^ previewBrick
]

{ #category : #accessing }
GTSpotterPaneBrick >> previewBrick: aBrick [

	previewBrick ifNotNil: [self removeBrick: aBrick].
	previewBrick := aBrick.
	
	self themer spotterThemer spotterPreviewStyleFor: previewBrick
]

{ #category : #action }
GTSpotterPaneBrick >> showPreview [
	
	self isPreviewShown ifTrue: [ ^ self ].
	
	self addBrickBack: self previewBrick.
	self previewBrick spotterModel ifNotNil: [ :aSpotter |
		aSpotter hasSelected ifTrue: [
			self previewBrick updatePreviewWith: aSpotter currentStep selected candidate ] ].
	
	self contentsBrick hSpaceFill: (100 - self previewBrick layout hPercent).
	self changed.
]

{ #category : #accessing }
GTSpotterPaneBrick >> spotterModel: aSpotter [

	self contentsBrick spotterModel: aSpotter.
	self arrowBrick spotterModel: aSpotter.
	self previewBrick spotterModel: aSpotter
]
