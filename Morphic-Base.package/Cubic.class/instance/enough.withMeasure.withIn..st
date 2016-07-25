cubic support
enough: nTry withMeasure: lastMeasure withIn: closeEnough
	"See comment in calcEnoughSegments for which I am a helper"
	
	| measure |
	measure := self measureFor: nTry.
	measure > (lastMeasure + closeEnough)
		ifFalse: [^ nTry // 2].
	^ self
		enough: 2 * nTry
		withMeasure: measure
		withIn: closeEnough