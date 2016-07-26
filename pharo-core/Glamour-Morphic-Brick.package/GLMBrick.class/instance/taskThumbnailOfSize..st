morph-common
taskThumbnailOfSize: thumbExtent
	"Answer a new task thumbnail for the receiver."

	|f t r|
	r := self globalBounds scaledAndCenteredIn: (0@0 extent: thumbExtent).
	f := Form extent: r extent depth: Display depth.
	t := MatrixTransform2x3 withScale: f extent / self extent.
	f getCanvas
		transformBy: t
		clippingTo: f boundingBox
		during: [:c | c translateBy: self globalBounds topLeft negated during: [:ct | self fullDrawOn: ct]]
		smoothing: 2.
	^ImageMorph new
		form: f