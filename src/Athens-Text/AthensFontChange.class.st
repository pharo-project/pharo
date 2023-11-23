"
A command to change the currently selected font
"
Class {
	#name : 'AthensFontChange',
	#superclass : 'AthensTextDisplayCommand',
	#instVars : [
		'font'
	],
	#category : 'Athens-Text',
	#package : 'Athens-Text'
}

{ #category : 'instance creation' }
AthensFontChange class >> font: aFont [
	^ self new font: aFont
]

{ #category : 'accessing' }
AthensFontChange >> font [
	^ font
]

{ #category : 'accessing' }
AthensFontChange >> font: aFont [
	font := aFont
]

{ #category : 'rendering' }
AthensFontChange >> renderOn: anAthensTextRenderer [

	anAthensTextRenderer setCurrentFont: font
]
