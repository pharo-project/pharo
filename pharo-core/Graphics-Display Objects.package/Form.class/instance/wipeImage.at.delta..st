transitions
wipeImage: otherImage at: topLeft delta: delta
	"Display wipeImage: (Form fromDisplay: (40@40 extent: 300@300)) reverse
		at: 40@40 delta: 0@-2"

	self wipeImage: otherImage at: topLeft delta: delta clippingBox: nil.
