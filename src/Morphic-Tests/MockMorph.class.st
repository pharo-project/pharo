"
I am a subclass of morph used to `MouseClickState`
"
Class {
	#name : #MockMorph,
	#superclass : #Morph,
	#instVars : [
		'magicClick',
		'magicDoubleClick'
	],
	#category : #'Morphic-Tests-Event'
}

{ #category : #testing }
MockMorph >> hasMyMagicClickActivated [
	^ magicClick notNil
]

{ #category : #testing }
MockMorph >> hasMyMagicDoubleClickActivated [
	^ magicDoubleClick notNil
]

{ #category : #events }
MockMorph >> myMagicClick: event [
	magicClick := event
]

{ #category : #events }
MockMorph >> myMagicDoubleClick: event [
	magicDoubleClick := event
]
