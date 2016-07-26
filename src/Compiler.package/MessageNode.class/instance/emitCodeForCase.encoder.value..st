code generation
emitCodeForCase: stack encoder: encoder value: forValue

	| braceNode sizeStream allReturn |
	forValue ifFalse:
		[^super emitCodeForEffect: stack encoder: encoder].
	braceNode := arguments first.
	sizeStream := sizes readStream.
	receiver emitCodeForValue: stack encoder: encoder.
	"There must be at least one branch around the otherwise/caseError
	  so the decompiler can identify the end of the otherwise/caseError."
	allReturn := true. "assume every case ends with a return"
	braceNode casesForwardDo:
		[:keyNode :valueNode :last | | thenSize elseSize |
		thenSize := sizeStream next.
		elseSize := sizeStream next.
		last ifFalse: [encoder genDup. stack push: 1].
		keyNode emitCodeForEvaluatedValue: stack encoder: encoder.
		equalNode emitCode: stack args: 1 encoder: encoder.
		self emitCodeForBranchOn: false dist: thenSize pop: stack encoder: encoder.
		last ifFalse: [encoder genPop. stack pop: 1].
		valueNode emitCodeForEvaluatedValue: stack encoder: encoder.
		last ifTrue: [stack pop: 1].
		valueNode returns ifFalse:
			[self emitCodeForJump: elseSize encoder: encoder.
			 allReturn := false].
		(last and: [allReturn]) ifTrue:
			[self emitCodeForJump: elseSize encoder: encoder]].
	arguments size = 2
		ifTrue:
			[arguments last emitCodeForEvaluatedValue: stack encoder: encoder] "otherwise: [...]"
		ifFalse:
			[NodeSelf emitCodeForValue: stack encoder: encoder.
			caseErrorNode emitCode: stack args: 0 encoder: encoder]