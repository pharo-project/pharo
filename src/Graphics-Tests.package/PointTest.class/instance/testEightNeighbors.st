tests - point functions
testEightNeighbors

	| x y |
	x := Point new.
 	x := (1 @ 1).
	y := x eightNeighbors.
	self assert: y = { (2 @ 1) . (2 @ 2) . (1 @ 2) . (0 @ 2) . (0 @ 1) . (0 @ 0) . (1 @ 0) . (2 @ 0)}.
	x := Point new.
	x := -2 @ -6.
	y := x eightNeighbors.
	self assert: y = { (-1 @ -6) . (-1 @ -5) . (-2 @ -5) . (-3 @ -5) . (-3 @ -6) . (-3 @ -7) . (-2 @ -7) . (-1 @ -7)}.
