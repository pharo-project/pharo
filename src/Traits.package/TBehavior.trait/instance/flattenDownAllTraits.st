traits
flattenDownAllTraits
	self traitComposition allTraits do: [:each | self flattenDown: each].
	[ self traitComposition isEmpty ] assert.
	self traitComposition: nil.