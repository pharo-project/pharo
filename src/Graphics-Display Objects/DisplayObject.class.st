"
The abstract protocol for most display primitives that are used by Views for presenting information on the screen.
"
Class {
	#name : 'DisplayObject',
	#superclass : 'Object',
	#category : 'Graphics-Display Objects-Common',
	#package : 'Graphics-Display Objects',
	#tag : 'Common'
}

{ #category : 'transforming' }
DisplayObject >> align: alignmentPoint with: relativePoint [
	"Translate the receiver's offset such that alignmentPoint aligns with
	relativePoint."

	self offset: (self offset translateBy: relativePoint - alignmentPoint)
]

{ #category : 'accessing - display box' }
DisplayObject >> boundingBox [
	"Answer the rectangular area that represents the boundaries of the
	receiver's space of information."

	^self computeBoundingBox
]

{ #category : 'accessing - display box' }
DisplayObject >> center [

	^ self boundingBox center
]

{ #category : 'accessing - display box' }
DisplayObject >> computeBoundingBox [
	"Answer the rectangular area that represents the boundaries of the
	receiver's area for displaying information. This is the primitive for
	computing the area if it is not already known."

	self subclassResponsibility
]

{ #category : 'displaying - generic' }
DisplayObject >> displayOn: aDisplayMedium [
	"Simple default display in order to see the receiver in the upper left
	corner of screen."

	self displayOn: aDisplayMedium at: 0 @ 0
]

{ #category : 'displaying - generic' }
DisplayObject >> displayOn: aDisplayMedium at: aDisplayPoint [
	"Display the receiver located at aDisplayPoint with default settings for
	rule and halftone."

	self displayOn: aDisplayMedium
		at: aDisplayPoint
		clippingBox: aDisplayMedium boundingBox
		rule: Form over
		fillColor: nil
]

{ #category : 'displaying - generic' }
DisplayObject >> displayOn: aDisplayMedium at: aDisplayPoint clippingBox: clipRectangle [
	"Display the receiver located at aDisplayPoint with default settings for
	rule and halftone. Information to be displayed must be confined to the
	area that intersects with clipRectangle."

	self displayOn: aDisplayMedium
		at: aDisplayPoint
		clippingBox: clipRectangle
		rule: Form over
		fillColor: nil
]

{ #category : 'displaying - generic' }
DisplayObject >> displayOn: aDisplayMedium at: aDisplayPoint clippingBox: clipRectangle rule: ruleInteger fillColor: aForm [
	"This is the basic display primitive for graphic display objects. Display
	the receiver located at aDisplayPoint with rule, ruleInteger, and mask,
	aForm. Information to be displayed must be confined to the area that
	intersects with clipRectangle."

	self subclassResponsibility
]

{ #category : 'displaying - generic' }
DisplayObject >> displayOn: aDisplayMedium at: aDisplayPoint rule: ruleInteger [
	"Display the receiver located at aPoint with default setting for the
	halftone and clippingBox."

	self displayOn: aDisplayMedium
		at: aDisplayPoint
		clippingBox: aDisplayMedium boundingBox
		rule: ruleInteger
		fillColor: nil
]

{ #category : 'displaying - generic' }
DisplayObject >> displayOnPort: port at: location rule: rule [

	port copyForm: self to: location rule: rule
]

{ #category : 'accessing' }
DisplayObject >> extent [
	"Answer the point that represents the width and height of the receiver's
	bounding box."

	^self boundingBox extent
]

{ #category : 'accessing' }
DisplayObject >> height [
	"Answer the number that represents the height of the receiver's
	bounding box."

	^self boundingBox height
]

{ #category : 'accessing - display box' }
DisplayObject >> initialExtent [
	"Included here for when a FormView is being opened
	as a window.  (4@4) covers border widths."

	^ self extent + (4@4)
]

{ #category : 'testing' }
DisplayObject >> isTransparent [

	^ false
]

{ #category : 'accessing' }
DisplayObject >> offset [
	"Answer the amount by which the receiver should be offset when it is
	displayed or its position is tested."

	self subclassResponsibility
]

{ #category : 'accessing' }
DisplayObject >> offset: aPoint [
	"Set the amount by which the receiver's position is offset."

	^self
]

{ #category : 'accessing' }
DisplayObject >> relativeRectangle [
	"Answer a Rectangle whose top left corner is the receiver's offset position
	and whose width and height are the same as the receiver."

	^Rectangle origin: self offset extent: self extent
]

{ #category : 'truncation and round off' }
DisplayObject >> rounded [
	"Convert the offset of the receiver to integer coordinates."

	self offset: self offset rounded
]

{ #category : 'transforming' }
DisplayObject >> scaleBy: aPoint [
	"Scale the receiver's offset by aPoint."

	self offset: (self offset scaleBy: aPoint)
]

{ #category : 'transforming' }
DisplayObject >> translateBy: aPoint [
	"Translate the receiver's offset."

	self offset: (self offset translateBy: aPoint)
]

{ #category : 'accessing' }
DisplayObject >> width [
	"Answer the number that represents the width of the receiver's bounding
	box."

	^self boundingBox width
]
