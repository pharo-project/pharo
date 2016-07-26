geometry
computeSlider
	| r |
	r := self roomToMove.
	self descending
		ifFalse:
			[slider position: (bounds isWide
				ifTrue: [r topLeft + ((r width * value) asInteger @ 0)]
				ifFalse: [r topLeft + (0 @ (r height * value)  asInteger)])]
		ifTrue:
			[slider position: (bounds isWide
				ifTrue:	[r bottomRight - ((r width * value) asInteger @ 0)]
				ifFalse:	[r bottomRight - ((0 @ (r height * value) asInteger))])].
	slider extent: self sliderExtent