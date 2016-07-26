initialize-release
needsFrameSize: newFrameSize
	"Set the largeFrameBit to accomodate the newFrameSize"
	(self numTemps + newFrameSize) > LargeFrame ifTrue:
		[^ self error: 'Cannot compile -- stack including temps is too deep'].
	self setFrameBit: ((self numTemps + newFrameSize) > SmallFrame 
		or: [ self primitive = 84 "perform:withArguments:"])