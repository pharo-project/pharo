*Morphic-Base
scaledIntoFormOfSize: aNumberOrPoint 
	"Scale and center the receiver into a form of a given size"

	| extent scale scaledForm result |

	extent := aNumberOrPoint asPoint.
	extent = self extent ifTrue: [^ self].

	(self height isZero or: [self width isZero])
		ifTrue: [^ Form extent: extent depth: self depth].

	scale := extent y / self height min: extent x / self width.
	scaledForm := self
				magnify: self boundingBox
				by: scale
				smoothing: 8.

	result := Form extent: extent depth: 32.
	result getCanvas
		translucentImage: scaledForm
		at: extent - scaledForm extent // 2.

	^ result
