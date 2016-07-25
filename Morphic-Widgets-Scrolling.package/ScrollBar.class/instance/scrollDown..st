scrolling
scrollDown: count
	self setValue: (value + (scrollDelta * count) + 0.000001 min: 1.0)