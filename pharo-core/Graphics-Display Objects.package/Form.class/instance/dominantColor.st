analyzing
dominantColor
	| tally max maxi |
	self depth > 16 ifTrue:
		[^(self asFormOfDepth: 16) dominantColor].
	tally := self tallyPixelValues.
	max := maxi := 0.
	tally withIndexDo: [:n :i | n > max ifTrue: [max := n. maxi := i]].
	^ Color colorFromPixelValue: maxi - 1 depth: self depth