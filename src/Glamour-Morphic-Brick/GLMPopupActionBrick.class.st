"
I am a special popup brick designed to work with GLMPopupAction
"
Class {
	#name : #GLMPopupActionBrick,
	#superclass : #GLMEmptyPopupBrick,
	#instVars : [
		'onCreatedBlock'
	],
	#category : 'Glamour-Morphic-Brick-Widgets-UI-Popup'
}

{ #category : #initialization }
GLMPopupActionBrick >> initialize [
	super initialize.
	
	self themer: self themer actionThemer.
	onCreatedBlock := [  ]
]

{ #category : #initialization }
GLMPopupActionBrick >> onCreated [
	| theContentBricks |
	
	theContentBricks := onCreatedBlock brickValue: self withEnoughArguments: { self triggerBrick }.
	theContentBricks := theContentBricks asOrderedCollection select: #isNotNil.
	self contentBrick addAllBricks: theContentBricks
]

{ #category : #accessing }
GLMPopupActionBrick >> onCreated: aBlock [
	onCreatedBlock := aBlock
]
