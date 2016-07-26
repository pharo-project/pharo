scrolling
scrollUp: count
	self setValue: (value - (scrollDelta * count) - 0.000001 max: 0.0)