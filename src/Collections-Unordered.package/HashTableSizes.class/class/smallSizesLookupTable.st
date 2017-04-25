private
smallSizesLookupTable
	smallSizesLookupTable
		ifNil: [
			"Compute a lookup table of known good table sizes by caching the
			result of binary searching 'sizes' with #basicAtLeast: for a range
			of small sizes from 0 to #maxSmallSize."
			smallSizesLookupTable :=
				(0 to: self maxSmallSize) asArray collect: [ :each |
					self basicAtLeast: each ] ].
	^ smallSizesLookupTable