Class {
	#name : 'TextEditionEvent',
	#superclass : 'UserInputEvent',
	#instVars : [
		'text',
		'start',
		'length'
	],
	#category : 'Morphic-Core-Events',
	#package : 'Morphic-Core',
	#tag : 'Events'
}

{ #category : 'accessing' }
TextEditionEvent >> isTextEditionEvent [

	^true
]

{ #category : 'accessing' }
TextEditionEvent >> length [

	^ length
]

{ #category : 'accessing' }
TextEditionEvent >> length: anInteger [

	length := anInteger
]

{ #category : 'dispatching' }
TextEditionEvent >> sentTo: anObject [

	^anObject handleTextEditionEvent: self
]

{ #category : 'accessing' }
TextEditionEvent >> start [

	^ start
]

{ #category : 'accessing' }
TextEditionEvent >> start: anInteger [

	start := anInteger
]

{ #category : 'accessing' }
TextEditionEvent >> text [

	^ text
]

{ #category : 'accessing' }
TextEditionEvent >> text: aString [

	text := aString
]
