"
I am paint which using color gradient for fills.

The gradient is defined using color ramp in range between 0 and 1
where colors can be set at certain point. The color are interpolated between defined points.

The color ramp is a collection of associations with keys - floating point values between 0 and 1
and values with Colors, for example:

{  0 -> Color white. 1 -> Color black }.

My subclasses used to define a concrete kind of gradient,
while me is an abstract class which has common properties for all gradients: color ramp.

Also, note that my subclasses representing backend-neutral gradients,
encapsulating all necessary data, which is then passed to corresponding surface factory method, like
AthensSurface>>#createLinearGradient:start:stop: , which answers backend-specific gradient paint.
I am userful in cases when user wants to define a paint without need
to have any Athens surface at hand.

Sure thing, using backend-specific gradients is preferable, if possible,
because conversion to backend-specific paint can be costly. 
Especially if gradient is statically defined (ramp values and other parameters never changing) 
and used many times during the life time of surface.


"
Class {
	#name : #GradientPaint,
	#superclass : #AthensAbstractPaint,
	#instVars : [
		'colorRamp'
	],
	#category : #'Athens-Core-Paints'
}

{ #category : #accessing }
GradientPaint >> atStop: anOffset put: aColor [

	colorRamp add: (anOffset -> aColor)
]

{ #category : #accessing }
GradientPaint >> colorRamp [
	^ colorRamp
]

{ #category : #accessing }
GradientPaint >> colorRamp: aRamp [
	"The color ramp is a collection of associations with keys - floating point values between 0 and 1
	 and values with Colors, for example:

	{  0 -> Color white. 1 -> Color black }."

	colorRamp := aRamp copy
]

{ #category : #initialization }
GradientPaint >> initialize [
	super initialize.
	colorRamp := OrderedCollection new
]
