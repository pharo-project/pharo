"
Colour selector featuring a saturation/volume area and a hue selection strip.
"
Class {
	#name : #HSVColorSelectorMorph,
	#superclass : #Morph,
	#instVars : [
		'svMorph',
		'hMorph'
	],
	#category : 'Morphic-Widgets-ColorPicker'
}

{ #category : #accessing }
HSVColorSelectorMorph >> colorSelected: aColor [
	"A color has been selected. Make the hue match."

	"self hMorph value: aColor hue / 360.
	self svMorph basicColor: (Color h: aColor hue s: 1.0 v: 1.0)."
	self triggerEvent: #colorSelected with: aColor
]

{ #category : #accessing }
HSVColorSelectorMorph >> defaultColor [
	"Answer the default color/fill style for the receiver."
	
	^Color transparent

]

{ #category : #accessing }
HSVColorSelectorMorph >> hMorph [
	"Answer the value of hMorph"

	^ hMorph
]

{ #category : #accessing }
HSVColorSelectorMorph >> hMorph: anObject [
	"Set the value of hMorph"

	hMorph := anObject
]

{ #category : #accessing }
HSVColorSelectorMorph >> hue: aFloat [
	"Set the hue in the range 0.0 - 1.0. Update the SV morph and hMorph."

	self hMorph value: aFloat.
	self svMorph color: (Color h: aFloat * 359.9 s: 1.0 v: 1.0)
]

{ #category : #initialization }
HSVColorSelectorMorph >> initialize [
	"Initialize the receiver."

	super initialize.
	self
		borderWidth: 0;
		changeTableLayout;
		cellInset: 4;
		listDirection: #leftToRight;
		cellPositioning: #topLeft;
		svMorph: self newSVColorMorph;
		hMorph: self newHColorMorph;
		addMorphBack: self svMorph;
		addMorphBack: self hMorph;
		extent: 192@152;
		hue: 0.5
]

{ #category : #operations }
HSVColorSelectorMorph >> newHColorMorph [
	"Answer a new hue color morph."

	^HColorSelectorMorph new
		model: self;
		setValueSelector: #hue:;
		hResizing: #rigid;
		vResizing: #spaceFill;
		extent: 36@36
]

{ #category : #operations }
HSVColorSelectorMorph >> newSVColorMorph [
	"Answer a new saturation/volume color morph."

	^SVColorSelectorMorph new
		extent: 152@152;
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		when: #colorSelected send: #colorSelected: to: self
]

{ #category : #operations }
HSVColorSelectorMorph >> selectedColor [
	"Answer the selected color."

	^self svMorph selectedColor
]

{ #category : #operations }
HSVColorSelectorMorph >> selectedColor: aColor [
	"Set the hue and sv components."

	self hue: aColor hue / 360.
	self svMorph selectedColor: aColor
]

{ #category : #accessing }
HSVColorSelectorMorph >> svMorph [
	"Answer the value of svMorph"

	^ svMorph
]

{ #category : #accessing }
HSVColorSelectorMorph >> svMorph: anObject [
	"Set the value of svMorph"

	svMorph := anObject
]
