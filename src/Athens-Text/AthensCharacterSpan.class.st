"
A text command to render a character span containing the range of characters for with same attributes
"
Class {
	#name : #AthensCharacterSpan,
	#superclass : #AthensTextDisplayCommand,
	#instVars : [
		'start',
		'stop'
	],
	#category : #'Athens-Text'
}

{ #category : #rendering }
AthensCharacterSpan >> renderOn: renderer [
	renderer renderCharactersFrom: start to: stop
]

{ #category : #accessing }
AthensCharacterSpan >> splitAt: position [
	| newSpan |
	"split receiver on two character spans "
	" position points to first character which should be included in newly created span "
	
	position == start ifTrue: [ ^ self ].
	(position < start or: [ position > stop ]) ifTrue: [ self error: 'invalid position' ].

	newSpan := self class new
		start: position;
		stop: stop.

	stop := position - 1.
	self insert: newSpan.
]

{ #category : #accessing }
AthensCharacterSpan >> start [

	^ start
]

{ #category : #accessing }
AthensCharacterSpan >> start: anObject [

	start := anObject
]

{ #category : #accessing }
AthensCharacterSpan >> stop [

	^ stop
]

{ #category : #accessing }
AthensCharacterSpan >> stop: anObject [

	stop := anObject
]
