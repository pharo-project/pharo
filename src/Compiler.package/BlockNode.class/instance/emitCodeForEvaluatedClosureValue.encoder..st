code generation (closures)
emitCodeForEvaluatedClosureValue: stack encoder: encoder
	| position |
	position := stack position.
	stack position: arguments size + copiedValues size.
	temporaries size timesRepeat:
		[NodeNil emitCodeForValue: stack encoder: encoder].
	self
		reindexingLocalsDo: [self emitCodeForEvaluatedValue: stack encoder: encoder]
		encoder: encoder.
	self returns ifFalse:
		[encoder genReturnTopToCaller.
		 pc := encoder methodStreamPosition].
	stack position: position