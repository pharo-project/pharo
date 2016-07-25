private
updateExtent
	"Recalculate the current extent, based on lines width"
	| x y |
	
	x := y := 0.
	lines do: [:each |
		x := x max: each width.
		y := y + each height.
	].

	extent := x@y