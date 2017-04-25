private
sizes
	sizes
		ifNil: [
			"Compute a sorted Array of known good table sizes that can be
			binary searched with #basicAtLeast:."
			sizes := self new computeSizes ].
	^ sizes