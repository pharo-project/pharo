"
I represent a linear gradient paint.
My start and stop points defining 
the position and direction of gradient fill 
in user space coordinates,
so that the ramp color at 0 maps to starting point
and ramp color at 1 maps to stop point of linear gradient.



Also check the comment of my superclass to know better how and when you can use me.

"
Class {
	#name : #LinearGradientPaint,
	#superclass : #GradientPaint,
	#instVars : [
		'start',
		'stop'
	],
	#category : #'Athens-Core-Paints'
}

{ #category : #'instance creation' }
LinearGradientPaint class >> from: aStartPoint  to: aStopPoint [
	^ self new start: aStartPoint; stop: aStopPoint; yourself
]

{ #category : #converting }
LinearGradientPaint >> asAthensPaintOn: aCanvas [
	^ aCanvas surface
			createLinearGradient: self colorRamp
				start: self start
				stop: self stop
]

{ #category : #accessing }
LinearGradientPaint >> start [

	^ start
]

{ #category : #accessing }
LinearGradientPaint >> start: anObject [

	start := anObject
]

{ #category : #accessing }
LinearGradientPaint >> stop [

	^ stop
]

{ #category : #accessing }
LinearGradientPaint >> stop: anObject [

	stop := anObject
]
