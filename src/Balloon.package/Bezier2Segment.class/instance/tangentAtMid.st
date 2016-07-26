vector functions
tangentAtMid
	"Return the tangent at the given parametric value along the receiver"
	| in out |
	in := self tangentAtStart.
	out := self tangentAtEnd.
	^in + out * 0.5