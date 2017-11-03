Class {
	#name : #DiffMapMorph,
	#superclass : #BorderedMorph,
	#instVars : [
		'mappings'
	],
	#category : #Tool-Diff
}

{ #category : #accessing }
DiffMapMorph >> adoptPaneColor: paneColor [
	"Change our border color too."
	
	|c|
	super adoptPaneColor: paneColor.
	paneColor ifNil: [^self].
	c := paneColor alphaMixed: 0.1 with: self theme diffMapColor.
	self fillStyle: ((GradientFillStyle
			ramp: (self gradientRampForColor: c))
		origin: self bounds topLeft;
		direction: 0@ self height).
	self borderStyle baseColor: paneColor
]

{ #category : #accessing }
DiffMapMorph >> defaultColor [
	"Answer the default color for the receiver."
	^ self theme diffMapColor
]

{ #category : #drawing }
DiffMapMorph >> drawOn: aCanvas [
	"Draw the indicators for the mappings."
	
	|b f|
	b := self innerBounds insetBy: 2.
	super drawOn: aCanvas.
	b height < 1 ifTrue: [^self].
	f := self mappingsHeight.
	f < 1 ifTrue: [^self].
	f := b height / f.
	aCanvas clipBy: self clippingBounds during: [:c |
		self mappings do: [:j |
			j drawMapOn: c in: b scale: f]]
]

{ #category : #drawing }
DiffMapMorph >> drawOnAthensCanvas: anAthensCanvas [
	"Draw the indicators for the mappings."
	
	|b f|
	b := self innerBounds insetBy: 2.
	super drawOnAthensCanvas: anAthensCanvas.
	b height < 1 ifTrue: [^self].
	f := self mappingsHeight.
	f < 1 ifTrue: [^self].
	f := b height / f.
	anAthensCanvas asCanvasWrapper clipBy: self clippingBounds during: [:c |
		self mappings do: [:j |
			j drawMapOn: c in: b scale: f]]
]

{ #category : #accessing }
DiffMapMorph >> extent: newExtent [
	"Update the gradient."
	
	super extent: newExtent.
	(self fillStyle notNil and: [self fillStyle isOrientedFill])
		ifTrue: [self fillStyle direction: 0@self height]
]

{ #category : #accessing }
DiffMapMorph >> gradientRampForColor: c [
	"Answer the background gradient ramp to use."
	
	^{0.0->c darker duller. 0.1-> c lighter.
		0.9->c twiceLighter. 1.0->c darker}
]

{ #category : #'event handling' }
DiffMapMorph >> handlesMouseDown: anEvent [
	"Answer true to report mouse down activity."
	
	^true
]

{ #category : #initialization }
DiffMapMorph >> initialize [
	"Initialize the receiver."

	super initialize.
	self
		mappings: #()
]

{ #category : #accessing }
DiffMapMorph >> mappings [
	"Answer the value of mappings"

	^ mappings
]

{ #category : #accessing }
DiffMapMorph >> mappings: anObject [
	"Set the value of mappings"

	mappings := anObject.
	self changed
]

{ #category : #'private - accessing' }
DiffMapMorph >> mappingsHeight [
	"Answer the maximum y of all the mappings."
	
	self mappings ifEmpty: [^0].
	^self mappings last dst range last 
]

{ #category : #'event handling' }
DiffMapMorph >> mouseDown: event [
	"Trigger a 0..1 range based on location."
	
	|b|
	b := self innerBounds insetBy: 2.
	self
		triggerEvent: #mapClicked
		with: (((event cursorPoint y asFloat - b top / b height) 
				min: 1) max: 0)
]
