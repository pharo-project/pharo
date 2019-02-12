"
I representing an elliptical arc, using endpoint parametrization:

- initial endpoint (x0,y0), which comes from previous segment
- final endpoint (x1,y1)

- rh, rv , the radii of unrotated ellipse

- Large arc flag: true if more than 180 degrees of the arc is to be traversed (as measured on the unscaled circle), false otherwise

- Sweep flag: true if the arc is to be traversed in the counter-clockwise direction, false otherwise

(not functional yet)
"
Class {
	#name : #AthensEllipticalArcSegment,
	#superclass : #AthensPathSegment,
	#category : #'Athens-Core-Paths'
}

{ #category : #visitor }
AthensEllipticalArcSegment >> accept: aVisitor [
	^ aVisitor ellipticalArcSegment: self
]
