"
I representing a single line of text
"
Class {
	#name : #AthensTextLine,
	#superclass : #Object,
	#instVars : [
		'text',
		'startIndex',
		'endIndex',
		'height',
		'maxAscent',
		'maxDescent',
		'width',
		'top',
		'commands',
		'tail'
	],
	#category : #'Athens-Text'
}

{ #category : #accessing }
AthensTextLine >> addCommand: aCommand [

	commands 
		ifNil: [ tail := commands := aCommand. ]
		ifNotNil: [ tail next: aCommand ].
	tail := tail tail.

]

{ #category : #accessing }
AthensTextLine >> addWidth: aNumber [
	width := width + aNumber.
]

{ #category : #accessing }
AthensTextLine >> bottom [
	^ top + self heightAndDescent
]

{ #category : #accessing }
AthensTextLine >> commands [

	^ commands
]

{ #category : #accessing }
AthensTextLine >> commands: anObject [

	commands := anObject
]

{ #category : #accessing }
AthensTextLine >> endIndex [

	^ endIndex
]

{ #category : #accessing }
AthensTextLine >> endIndex: anObject [

	endIndex := anObject
]

{ #category : #accessing }
AthensTextLine >> height [
	^ height
]

{ #category : #accessing }
AthensTextLine >> height: anObject [

	height := anObject
]

{ #category : #accessing }
AthensTextLine >> heightAndDescent [

	^ height + maxDescent
]

{ #category : #initialization }
AthensTextLine >> initialize [
	width := height := maxAscent := maxDescent := 0
]

{ #category : #accessing }
AthensTextLine >> maxAscent [
	^ maxAscent
]

{ #category : #accessing }
AthensTextLine >> maxAscent: mascent [
	maxAscent := mascent max: maxAscent
]

{ #category : #accessing }
AthensTextLine >> maxDescent [
	^ maxDescent
]

{ #category : #accessing }
AthensTextLine >> maxDescent: mascent [

	maxDescent := mascent max: maxDescent
]

{ #category : #accessing }
AthensTextLine >> maxHeight: aHeight [
	height := height max: aHeight
]

{ #category : #rendering }
AthensTextLine >> renderOn: aTextRenderer [
	
	commands ifNil: [ ^ self ]. 
	commands do: [ :cmd |
		cmd renderOn: aTextRenderer
	].

]

{ #category : #rendering }
AthensTextLine >> renderOn: anAthensTextRenderer x: aSmallInteger y: aSmallInteger3 [ 
	self shouldBeImplemented
]

{ #category : #accessing }
AthensTextLine >> startIndex [

	^ startIndex
]

{ #category : #accessing }
AthensTextLine >> startIndex: index [
	startIndex := index
]

{ #category : #accessing }
AthensTextLine >> text [

	^ text
]

{ #category : #accessing }
AthensTextLine >> text: aText [ 
	text := aText
]

{ #category : #accessing }
AthensTextLine >> top [
	^ top
]

{ #category : #accessing }
AthensTextLine >> top: aTop [
	top := aTop
]

{ #category : #accessing }
AthensTextLine >> width [
	^ width
]

{ #category : #accessing }
AthensTextLine >> width: anObject [

	width := anObject
]
