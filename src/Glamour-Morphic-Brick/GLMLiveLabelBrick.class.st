"
A GLMLiveLabelBrick is simple extended label brick that updates text every world step. Text logic can be set by sending textLogic: message to me.

Instance Variables
	textLogic:		<BlockClosure>

textLogic
	- defines text updating logic

"
Class {
	#name : #GLMLiveLabelBrick,
	#superclass : #GLMLabelBrick,
	#instVars : [
		'textLogic'
	],
	#category : #Glamour-Morphic-Brick-Widgets-UI
}

{ #category : #initialization }
GLMLiveLabelBrick >> initialize [
	super initialize.
	textLogic := [ :x | x ]
]

{ #category : #'stepping and presenter' }
GLMLiveLabelBrick >> step [
	|newText|
	newText := self textLogic value asString.
	self text = newText ifTrue:  [ ^ self ].
	self text: newText.
	self doLayoutForce
]

{ #category : #accessing }
GLMLiveLabelBrick >> textLogic [ 
	^ textLogic
]

{ #category : #accessing }
GLMLiveLabelBrick >> textLogic: aBlock [
	textLogic := aBlock
]

{ #category : #testing }
GLMLiveLabelBrick >> wantsSteps [
	^ true
]
