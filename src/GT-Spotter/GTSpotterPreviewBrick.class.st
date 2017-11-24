Class {
	#name : #GTSpotterPreviewBrick,
	#superclass : #GLMBrick,
	#instVars : [
		'compositeBrick',
		'spotterModel',
		'asyncTask',
		'previewObject'
	],
	#category : #'GT-Spotter-UI'
}

{ #category : #accessing }
GTSpotterPreviewBrick >> asyncTask [

	^ asyncTask ifNil: [ asyncTask := GLMAsyncTask new ]
]

{ #category : #accessing }
GTSpotterPreviewBrick >> compositeBrick [

	^ compositeBrick
]

{ #category : #accessing }
GTSpotterPreviewBrick >> compositeBrick: aBrick [

	self compositeBrick ifNotNil: [ self removeBrick: self compositeBrick ].
	compositeBrick := aBrick.
	self themer spotterThemer spotterPreviewCompositeStyleFor: self compositeBrick.
	self addBrickBack: self compositeBrick.
	self doLayoutForce
]

{ #category : #initialization }
GTSpotterPreviewBrick >> defaultNotAvailableText [

	^ 'Preview is not available :('
]

{ #category : #updating }
GTSpotterPreviewBrick >> handleException: exception forComposite: composite [ 

	('[Spotter] Exception while building the preview content: ', exception asString) logCr.
	composite removeAllBricks.
]

{ #category : #initialization }
GTSpotterPreviewBrick >> initialize [
	super initialize.
	
	self compositeBrick: self newCompositeBrick
]

{ #category : #'instance creation' }
GTSpotterPreviewBrick >> newCompositeBrick [

	^ GLMCompositeBrick new
]

{ #category : #'event handling' }
GTSpotterPreviewBrick >> onCandidateSelected: ann [

	self updatePreviewWith: ann selected
]

{ #category : #'event handling' }
GTSpotterPreviewBrick >> onSearchFinished [

	self spotterModel currentStep candidates isEmpty
		ifTrue: [ self updatePreviewWithEmpty ]
]

{ #category : #'event handling' }
GTSpotterPreviewBrick >> onStepAdded [

	previewObject := nil.
	self updatePreviewWithLoading
]

{ #category : #'event handling' }
GTSpotterPreviewBrick >> onStepPopped [

	self updatePreviewWith: self spotterModel steps last selected candidate
]

{ #category : #accessing }
GTSpotterPreviewBrick >> onThemerChanged [
	super onThemerChanged.
	
	self themer spotterThemer spotterPreviewWidgetStyleFor: self
]

{ #category : #accessing }
GTSpotterPreviewBrick >> spotterModel [

	^ spotterModel
]

{ #category : #accessing }
GTSpotterPreviewBrick >> spotterModel: aSpotter [
	
	spotterModel := aSpotter.
	aSpotter announcer when: GTSpotterSelectedCandidateChanged send: #onCandidateSelected: to: self.
	aSpotter announcer when: GTSpotterStepPopped send: #onStepPopped to: self.
	aSpotter announcer when: GTSpotterStepAdded send: #onStepAdded to: self.
	aSpotter announcer when: GTSpotterSearchFinished send: #onSearchFinished to: self.
	
	
]

{ #category : #updating }
GTSpotterPreviewBrick >> updatePreviewWith: anObject [
	|wasNil|
	self spotterModel isPreviewVisible ifFalse: [ ^ self ].
	
	wasNil := previewObject isNil.
	previewObject = anObject ifTrue: [ ^ self ].
	previewObject := anObject.
	previewObject ifNil: [ ^ self updatePreviewWithEmpty ].
	wasNil ifTrue: [ self updatePreviewWithLoading ].
	
	self asyncTask
		execute: [ :composite :entity |
				[ entity spotterPreviewIn: composite inContext: self spotterModel ]
					on: Error
					do: [ :exception |
						self spotterModel exceptionHandler 
							handleException: exception 
							do: [ self handleException: exception forComposite: composite ] ].
				 composite ]
		withArguments: { self newCompositeBrick . anObject }
		callback: [ :composite |
			self compositeBrick: composite.
			self changed ]
		priority: Processor userBackgroundPriority
		delay: 50
]

{ #category : #updating }
GTSpotterPreviewBrick >> updatePreviewWithEmpty [

	self removeAllBricks.
	self asyncTask
		execute: [ | emptyBrick |
			emptyBrick := GLMLabelBrick new
				text: self defaultNotAvailableText;
				textPosition: #center;
				textColor: Color lightGray;
					with: [ :b | b label
					customFont;
					fontSize: 14 ] ]
		withArguments: {  }
		callback: [ :brick | self compositeBrick: brick ]
		priority: Processor userBackgroundPriority
		delay: 50
]

{ #category : #updating }
GTSpotterPreviewBrick >> updatePreviewWithLoading [

	self compositeBrick: (
		GLMLabelBrick new
			text: 'Loading...';
			textPosition: #center;
			textColor: Color lightGray;
				with: [ :b | b label
				customFont;
				fontSize: 14 ])
]
