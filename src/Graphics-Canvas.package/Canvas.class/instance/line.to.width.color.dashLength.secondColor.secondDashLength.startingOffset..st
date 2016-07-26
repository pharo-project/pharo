drawing
line: pt1 to: pt2 width: width color: color1 dashLength: s1 secondColor: color2 secondDashLength: s2 startingOffset: startingOffset 
	"Draw a line using the given width, colors and dash lengths.
	Originally written by Stephan Rudlof; tweaked by Dan Ingalls
	to use startingOffset for sliding offset as in 'ants' animations.
	Returns the sum of the starting offset and the length of this line."

	| dist deltaBig colors nextPhase segmentOffset phase segmentLength startPoint distDone endPoint segLens |
	dist := pt1 dist: pt2.
	dist = 0 ifTrue: [^startingOffset].
	s1 = 0 & (s2 = 0) ifTrue: [^startingOffset].
	deltaBig := pt2 - pt1.
	colors := { 
				color1.
				color2}.
	segLens := { 
				s1 asFloat.
				s2 asFloat}.
	nextPhase := { 
				2.
				1}.

	"Figure out what phase we are in and how far, given startingOffset."
	segmentOffset := startingOffset \\ (s1 + s2).
	segmentLength := segmentOffset < s1 
		ifTrue: 
			[phase := 1.
			s1 - segmentOffset]
		ifFalse: 
			[phase := 2.
			 s1 + s2 - segmentOffset].
	startPoint := pt1.
	distDone := 0.0.
	[distDone < dist] whileTrue: 
			[segmentLength := segmentLength min: dist - distDone.
			endPoint := startPoint + (deltaBig * segmentLength / dist).
			self 
				line: startPoint truncated
				to: endPoint truncated
				width: width
				color: (colors at: phase).
			distDone := distDone + segmentLength.
			phase := nextPhase at: phase.
			startPoint := endPoint.
			segmentLength := segLens at: phase].
	^startingOffset + dist