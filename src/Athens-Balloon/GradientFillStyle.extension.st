Extension { #name : #GradientFillStyle }

{ #category : #'*Athens-Balloon' }
GradientFillStyle >> asAthensPaintOn: anAthensCanvas [

	^ AthensGradientPaint new gradient: self.
	"^ radial ifTrue: [
		anAthensCanvas surface
			createRadialGradient: colorRamp
			center:  (anAthensCanvas pathTransform inverseTransform: origin )
			radius: direction y ]
	ifFalse: [
		anAthensCanvas surface
			createLinearGradient: colorRamp
			origin: (anAthensCanvas pathTransform inverseTransform: origin)
			corner: (anAthensCanvas pathTransform inverseTransform: origin + direction ) ]
"
]
