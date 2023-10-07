Class {
	#name : 'TextInputEvent',
	#superclass : 'UserInputEvent',
	#instVars : [
		'text'
	],
	#category : 'Morphic-Core-Events',
	#package : 'Morphic-Core',
	#tag : 'Events'
}

{ #category : 'testing' }
TextInputEvent >> isTextInputEvent [

	^true
]

{ #category : 'dispatching' }
TextInputEvent >> sentTo: anObject [

	^anObject handleTextInputEvent: self
]

{ #category : 'accessing' }
TextInputEvent >> text [
	^ text
]

{ #category : 'accessing' }
TextInputEvent >> text: aString [

	text := aString
]
