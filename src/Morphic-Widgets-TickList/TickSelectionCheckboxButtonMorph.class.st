"
A TickSelectionCheckboxButtonMorph is the button hidden into the TickSelectionCheckboxMorph
"
Class {
	#name : #TickSelectionCheckboxButtonMorph,
	#superclass : #CheckboxButtonMorph,
	#instVars : [
		'index'
	],
	#category : 'Morphic-Widgets-TickList'
}

{ #category : #accessing }
TickSelectionCheckboxButtonMorph >> index: anIndex [

	index := anIndex
]

{ #category : #event }
TickSelectionCheckboxButtonMorph >> mouseUp: evt [
	"Handle the transitions."
	| result |
	result := super mouseUp: evt.
	self pressed: index.
	^ result 
]

{ #category : #event }
TickSelectionCheckboxButtonMorph >> pressed: anIndex [

	self owner pressed: anIndex
]
