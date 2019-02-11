"
Text display command for changing the background color
"
Class {
	#name : #AthensBackgroundChange,
	#superclass : #AthensTextDisplayCommand,
	#instVars : [
		'color',
		'start',
		'width'
	],
	#category : #'Athens-Text'
}

{ #category : #adding }
AthensBackgroundChange >> addWidth: aWidth [
	width := width + aWidth
]

{ #category : #accessing }
AthensBackgroundChange >> color [

	^ color
]

{ #category : #accessing }
AthensBackgroundChange >> color: anObject [

	color := anObject
]

{ #category : #initialization }
AthensBackgroundChange >> initialize [
	width :=0.
	color := Color transparent.
]

{ #category : #rendering }
AthensBackgroundChange >> renderOn: renderer [
	color isTransparent ifFalse: [
		renderer renderBackground: self
	]
]

{ #category : #accessing }
AthensBackgroundChange >> start [
	^ start
]

{ #category : #accessing }
AthensBackgroundChange >> start: aStart [
	start := aStart
]

{ #category : #accessing }
AthensBackgroundChange >> width [

	^ width
]

{ #category : #accessing }
AthensBackgroundChange >> width: anObject [

	width := anObject
]
