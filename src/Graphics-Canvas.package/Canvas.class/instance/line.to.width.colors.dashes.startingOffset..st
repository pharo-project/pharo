drawing
line: pt1 to: pt2 width: width colors: colors dashes: dashes startingOffset: startingOffset 
	"Draw a line using the given width, colors and dash lengths.
	Dash lengths are considered as multiples of width."

	|dist deltaBig segmentOffset phase segmentLength startPoint distDone endPoint segLens lens l ep|
	width = 0 ifTrue: [^startingOffset].
	dist := pt1 distanceTo: pt2.
	dist = 0 ifTrue: [^startingOffset].
	(dashes allSatisfy: [:d | d = 0]) ifTrue: [^startingOffset].
	deltaBig := pt2 - pt1.
	segLens := dashes collect: [:d | d * width].
	
	"Figure out what phase we are in and how far, given startingOffset."
	segmentOffset := startingOffset \\ segLens sum.
	lens := segLens readStream.
	l := 0.
	[lens atEnd or: [segmentOffset <= (l := l + lens next)]] whileFalse: [].
	segmentLength := lens atEnd
		ifTrue: [phase := segLens size.
				segLens sum - segmentOffset]
		ifFalse: [phase := lens position.
				(segLens first: phase) sum - segmentOffset.].
	startPoint := pt1.
	distDone := 0.0.
	segmentLength < (segLens at: phase)
		ifTrue: [startPoint := startPoint + (deltaBig * segmentLength / dist).
				distDone := distDone + segmentLength.
				phase := phase \\ segLens size + 1.
				segmentLength :=  (segLens at: phase)].
	[distDone < dist] whileTrue: 
			[segmentLength := segmentLength min: dist - distDone.
			endPoint := startPoint + (deltaBig * segmentLength / dist).
			ep := startPoint + (deltaBig * (segmentLength - width max: 0) / dist).
			self 
				line: startPoint truncated
				to: ep truncated
				width: width
				color: (colors at: phase).
			distDone := distDone + segmentLength.
			phase := phase \\ segLens size + 1.
			startPoint := endPoint.
			segmentLength := segLens at: phase].
	^startingOffset + distDone