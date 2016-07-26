morphing
morphFrom: srcShape to: dstShape at: ratio
	| scale unscale srcPoints dstPoints pt1 pt2 x y |
	scale := (ratio * 1024) asInteger.
	scale < 0 ifTrue:[scale := 0].
	scale > 1024 ifTrue:[scale := 1024].
	unscale := 1024 - scale.
	srcPoints := srcShape points.
	dstPoints := dstShape points.
	1 to: points size do:[:i|
		pt1 := srcPoints at: i.
		pt2 := dstPoints at: i.
		x := ((pt1 x * unscale) + (pt2 x * scale)) bitShift: -10.
		y := ((pt1 y * unscale) + (pt2 y * scale)) bitShift: -10.
		points at: i put: x@y].