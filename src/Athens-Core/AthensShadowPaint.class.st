"
I representing a shadow paint object used by Athens.
My subclasses provide backend-specific implementation.
I am more serving to define the common protocols and
requirements for all backends (such as defaults).


IMPORTANT NOTE: a shadow paint object provides a protocol 
only for setting shadow properties, but not retrieving them back.
This is intentionally, because the way how these properties is
managed are highly backend-specific. 

The shadow paint has following properties:
 - color. A shadow color defined in RGBA color space.

	Default: Color black alpha: 0.5

 - shadow width. Can be set using #width:

	Default: if width is not set explicitly for shadow paint,
		it is assumed to be equal to 2.0.
		
 - shadow offset. Defines how far shadow is located.
	Shadow offset is a point where x component defines ofset on x-asix and y component on y-axis.
	
	Default: if offset is not set  explicitely it is assumed to be equal 2@2.
			
 - shadow blur. Defines how smooth shadow is.
	Blur is a point with x component defining bluring along x-axis and y component along y axis
	
	Default: if blur is not set explicitely it is assumed to be equal 4@4

"
Class {
	#name : #AthensShadowPaint,
	#superclass : #AthensPaint,
	#instVars : [
		'color',
		'width',
		'blur',
		'offset'
	],
	#category : #'Athens-Core-Paints'
}

{ #category : #converting }
AthensShadowPaint >> asStrokePaintOn: aCanvas [
	"receiver can be converted to stroke paint
	presering color and width"
	^ (aCanvas surface createStrokePaintFor: self color)
		width: self width
]

{ #category : #accessing }
AthensShadowPaint >> blur [
	^ blur
]

{ #category : #accessing }
AthensShadowPaint >> blur: aPoint [
	"Set shadpw blur"
	blur := aPoint
]

{ #category : #accessing }
AthensShadowPaint >> color [
	^ color
]

{ #category : #accessing }
AthensShadowPaint >> color: aColor [
	color := aColor
]

{ #category : #initialization }
AthensShadowPaint >> initialize [
	super initialize.
	self setDefaults
]

{ #category : #accessing }
AthensShadowPaint >> offset [
	^ offset
]

{ #category : #accessing }
AthensShadowPaint >> offset: aPoint [
	"Set shadow offset"
	offset := aPoint
]

{ #category : #initialization }
AthensShadowPaint >> setDefaults [

	"Set the default values for shadows"
	self
		color: (Color black alpha: 0.5);
		width: 2;
		offset: 2@2;
		blur: 4@4
]

{ #category : #accessing }
AthensShadowPaint >> width [
	^ width
]

{ #category : #accessing }
AthensShadowPaint >> width: aNumber [
	"Set shadow width"
	width := aNumber
]
