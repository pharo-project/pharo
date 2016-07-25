composition
recomposeFrom: start to: stop delta: delta
	self
		disableDrawingWhile: [ 	self composer recomposeFrom: start to: stop delta: delta ]