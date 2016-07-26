computing
subdivideToBeMonoton
	"Subdivide the receiver at it's extreme point"
	| v1 v2 t other |
	v1 := (via - start).
	v2 := (end - via).
	t := (v1 y / (v2 y - v1 y)) negated asFloat.
	other := self subdivideAt: t.
	self assert: [self isMonoton].
	self assert: [other isMonoton].
	^other