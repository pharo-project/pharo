"
A command to change the current font color
"
Class {
	#name : #AthensColorChange,
	#superclass : #AthensTextDisplayCommand,
	#instVars : [
		'color'
	],
	#category : #Athens-Text
}

{ #category : #accessing }
AthensColorChange >> color [

	^ color
]

{ #category : #accessing }
AthensColorChange >> color: anObject [

	color := anObject
]

{ #category : #rendering }
AthensColorChange >> renderOn: renderer [
	renderer setCurrentColor: color
]
