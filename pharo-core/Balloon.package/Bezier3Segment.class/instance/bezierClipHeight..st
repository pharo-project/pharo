bezier clipping
bezierClipHeight: dir
	"Check if the argument overlaps the receiver somewhere 
	along the line from start to end. Optimized for speed."
	| u dirX dirY dx dy uMin uMax |
	dirX := dir x.
	dirY := dir y.
	uMin := 0.0.
	uMax := (dirX * dirX) + (dirY * dirY).

	dx := via1 x - start x.
	dy := via1 y - start y.
	u := (dirX * dx) + (dirY * dy).
	u < uMin ifTrue:[uMin := u].
	u > uMax ifTrue:[uMax := u].

	dx := via2 x - start x.
	dy := via2 y - start y.
	u := (dirX * dx) + (dirY * dy).
	u < uMin ifTrue:[uMin := u].
	u > uMax ifTrue:[uMax := u].

	^uMin@uMax