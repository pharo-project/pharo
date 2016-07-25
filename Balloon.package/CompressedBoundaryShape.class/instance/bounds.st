accessing
bounds
	| min max width |
	points isEmpty ifTrue:[^0@0 corner: 1@1].
	min := max := points first.
	points do:[:pt|
		min := min min: pt.
		max := max max: pt
	].
	width := 0.
	lineWidths valuesDo:[:w| width := width max: w].
	^(min corner: max) insetBy: (width negated asPoint)