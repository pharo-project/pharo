Class {
	#name : #GTSpotterContentsBrick,
	#superclass : #GLMBrick,
	#instVars : [
		'headerBrick',
		'dropDownBrick',
		'spotterModel',
		'helpBrick'
	],
	#category : #'GT-Spotter-UI-Widgets'
}

{ #category : #accessing }
GTSpotterContentsBrick >> dropDownBrick [
	^ dropDownBrick
]

{ #category : #accessing }
GTSpotterContentsBrick >> dropDownBrick: aBrick [
	dropDownBrick ifNotNil: [self removeBrick: dropDownBrick].
	dropDownBrick := aBrick asBrick.
	
	dropDownBrick morph pagerModel: self spotterModel pagerModel.
	self themer spotterThemer dropDownPaneStyleFor: dropDownBrick.
	
	self addBrickBack: dropDownBrick.
	"aBrick openInWindow"
	self helpBrick: self newHelpBrick


]

{ #category : #accessing }
GTSpotterContentsBrick >> headerBrick [
	^ headerBrick
]

{ #category : #accessing }
GTSpotterContentsBrick >> headerBrick: aBrick [
	
	headerBrick ifNotNil: [self removeBrick: headerBrick].
	headerBrick := aBrick.
	
	self themer spotterThemer headerStyleFor: headerBrick.
	
	self addBrickBack: headerBrick
]

{ #category : #accessing }
GTSpotterContentsBrick >> helpBrick: aBrick [
	
	helpBrick ifNotNil: [self removeBrick: helpBrick].
	helpBrick := aBrick.
	
	self themer spotterThemer helpWidgetStyleFor: helpBrick.
	
	self addBrickBack: helpBrick
]

{ #category : #initialization }
GTSpotterContentsBrick >> initialize [
	super initialize.
		
	self headerBrick: self newHeaderBrick.

]

{ #category : #testing }
GTSpotterContentsBrick >> isDropDownPaneVisible [

	^ self dropDownBrick isNotNil
]

{ #category : #'instance creation' }
GTSpotterContentsBrick >> newDropDownMorph [

	^ GTSpotterDropDownMorph new
]

{ #category : #'instance creation' }
GTSpotterContentsBrick >> newHeaderBrick [

	^ GTSpotterHeaderBrick new
]

{ #category : #'instance creation' }
GTSpotterContentsBrick >> newHelpBrick [

	^ GTSpotterHelpBrick new
			spotter: spotterModel;
			yourself
		

]

{ #category : #'events-processing' }
GTSpotterContentsBrick >> onStepAdded [
	"as soon as step added I need to show drop down pane"
	self isDropDownPaneVisible ifTrue: [ ^ self ].
	
	self showDropDownPane
]

{ #category : #'brick-properties-events' }
GTSpotterContentsBrick >> onThemerChanged [
	super onThemerChanged.
	
	self themer spotterThemer spotterContentsWidgetStyleFor: self
]

{ #category : #actions }
GTSpotterContentsBrick >> showDropDownPane [

	self dropDownBrick: self newDropDownMorph.
	self dropDownBrick spotterModel: self spotterModel.
]

{ #category : #accessing }
GTSpotterContentsBrick >> spotterModel [

	^ spotterModel
]

{ #category : #accessing }
GTSpotterContentsBrick >> spotterModel: aModel [

	spotterModel := aModel.
	self headerBrick spotterModel: spotterModel.
	
	spotterModel steps isEmpty ifFalse: [ spotterModel currentStep hasCandidates ifTrue: [ self showDropDownPane ] ].

	spotterModel announcer when: GTSpotterStepAdded send: #onStepAdded to: self.
]

{ #category : #'events-processing' }
GTSpotterContentsBrick >> takeKeyboardFocus [

	self headerBrick takeKeyboardFocus
]
