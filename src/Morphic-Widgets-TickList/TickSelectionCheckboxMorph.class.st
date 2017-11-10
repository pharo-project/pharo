"
A TickSelectionCheckboxMorph is a checkbox which propagate a bit more the events
"
Class {
	#name : #TickSelectionCheckboxMorph,
	#superclass : #CheckboxMorph,
	#instVars : [
		'index'
	],
	#category : #Morphic-Widgets-TickList
}

{ #category : #accessing }
TickSelectionCheckboxMorph >> index: anIndex [

	index := anIndex
]

{ #category : #morph }
TickSelectionCheckboxMorph >> newButtonMorph [
	"Answer a new button morph"

	| instance |
	instance := TickSelectionCheckboxButtonMorph new
		target: self;
		actionSelector: #toggleSelected;
		vResizing: #shrinkWrap;
		hResizing: #shrinkWrap.
		
	instance index: index.
	^ instance
]

{ #category : #event }
TickSelectionCheckboxMorph >> pressed: anIndex [

	self owner owner pressed: anIndex
]
