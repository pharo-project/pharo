accessing
thumbnailOfSize: thumbExtent
	"Answer a new thumbnail for the receiver."

	|f t r|
	r := self header fullBounds scaledAndCenteredIn: (0@0 extent: thumbExtent).
	f := Form extent: r extent depth: Display depth.
	t := MatrixTransform2x3 withScale: 1.0.
	f getCanvas
		transformBy: t
		clippingTo: f boundingBox
		during: [:c | c translateBy: self visibleBounds topLeft negated during: [:ct | self container fullDrawOn: ct]]
		smoothing: 6.
	^ImageMorph new
		form: f