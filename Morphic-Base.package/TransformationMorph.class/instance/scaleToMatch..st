nil
scaleToMatch: aPoint
	| scaleFactor tfm originalScale |
	tfm := transform withScale: 1.0.
	originalScale := ((tfm localBoundsToGlobal: self renderedMorph fullBounds) corner -
		(tfm localPointToGlobal: self renderedMorph referencePosition)) r.
	"Catch cases where the reference point is on fullBounds corner"
	originalScale < 1.0 ifTrue:[originalScale := 1.0].
	scaleFactor := (aPoint - self referencePosition) r / originalScale.
	scaleFactor := scaleFactor < 1.0
		ifTrue: [scaleFactor detentBy: 0.05 atMultiplesOf: 0.25 snap: false]
		ifFalse: [scaleFactor detentBy: 0.1 atMultiplesOf: 0.5 snap: false].
	self adjustAfter:[self scale: ((scaleFactor min: 8.0) max: 0.1)].