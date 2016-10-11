drawing
line: pt1 to: pt2 width: w1 color: c1 stepWidth: s1 secondWidth: w2 secondColor: c2 secondStepWidth: s2 
	"Draw a line using the given width, colors and steps; both steps can  
	have different stepWidths (firstStep, secondStep), draw widths and  
	colors."
	| bigSteps offsetPoint dist p1p2Vec deltaBig delta1 delta2 lastPoint bigStep |
	s1 = 0 & (s2 = 0) ifTrue: [^ self].
	dist := pt1 distanceTo: pt2.
	dist = 0 ifTrue: [^ self].
	bigStep := s1 + s2.
	bigSteps := dist / bigStep.
	p1p2Vec := pt2 - pt1.
	deltaBig := p1p2Vec / bigSteps.
	delta1 := deltaBig * (s1 / bigStep).
	delta2 := deltaBig * (s2 / bigStep).
	dist <= s1
		ifTrue: 
			[self
				line: pt1 rounded
				to: pt2 rounded
				width: w1
				color: c1.
			^ self].
	0 to: bigSteps truncated - 1 do: 
		[:bigStepIx | 
		self
			line: (pt1 + (offsetPoint := deltaBig * bigStepIx)) rounded
			to: (pt1 + (offsetPoint := offsetPoint + delta1)) rounded
			width: w1
			color: c1.
		self
			line: (pt1 + offsetPoint) rounded
			to: (pt1 + (offsetPoint + delta2)) rounded
			width: w2
			color: c2].
	"if there was no loop, offsetPoint is nil"
	lastPoint := pt1 + ((offsetPoint ifNil: [0 @ 0])
					+ delta2).
	(lastPoint dist: pt2)
		<= s1
		ifTrue: [self
				line: lastPoint rounded
				to: pt2 rounded
				width: w1
				color: c1]
		ifFalse: 
			[self
				line: lastPoint rounded
				to: (lastPoint + delta1) rounded
				width: w1
				color: c1.
			self
				line: (lastPoint + delta1) rounded
				to: pt2
				width: w1
				color: c2]