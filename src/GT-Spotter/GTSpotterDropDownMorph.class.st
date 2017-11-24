Class {
	#name : #GTSpotterDropDownMorph,
	#superclass : #GLMPagerBrick,
	#instVars : [
		'spotterModel'
	],
	#category : #'GT-Spotter-UI'
}

{ #category : #building }
GTSpotterDropDownMorph >> buildPane: aStepModel [
	|pane|
	
	pane := GTSpotterResultsBrick new stepModel: aStepModel.
	self brickThemer spotterThemer resultsPaneStyleFor: pane.
	^ pane
]

{ #category : #initialization }
GTSpotterDropDownMorph >> initialize [
	super initialize.
	
	self showScrollBarWhenNeeded: false
]

{ #category : #'event handling' }
GTSpotterDropDownMorph >> onStepAdded: ann [
	"as soon as step added we need to push it"

	self pushPane: (self buildPane: ann stepModel)
]

{ #category : #accessing }
GTSpotterDropDownMorph >> spotterModel [
	^ spotterModel
]

{ #category : #accessing }
GTSpotterDropDownMorph >> spotterModel: aModel [ 
	spotterModel := aModel.
	
	spotterModel steps do: [ :each | self pushPane: (self buildPane: each) ].
	
	spotterModel announcer when: GTSpotterStepAdded send: #onStepAdded: to: self.
]
