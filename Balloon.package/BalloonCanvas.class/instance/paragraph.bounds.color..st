todo
paragraph: para bounds: bounds color: c
	(self ifNoTransformWithIn: bounds)
		ifTrue:[^super paragraph: para bounds: bounds color: c].