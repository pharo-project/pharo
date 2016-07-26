bezier clipping
bezierClipHeight: dir
	| dirX dirY uMin uMax dx dy u |
	dirX := dir x.
	dirY := dir y.
	uMin := 0.0.
	uMax := (dirX * dirX) + (dirY * dirY).
	dx := via x - start x.
	dy := via y - start y.
	u := (dirX * dx) + (dirY * dy).
	u < uMin ifTrue:[uMin := u].
	u > uMax ifTrue:[uMax := u].
	^uMin@uMax