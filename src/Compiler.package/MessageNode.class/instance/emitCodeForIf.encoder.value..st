code generation
emitCodeForIf: stack encoder: encoder value: forValue
	| thenExpr thenSize elseExpr elseSize |
	thenSize := sizes at: 1.
	elseSize := sizes at: 2.
	(forValue not and: [elseSize * thenSize > 0]) ifTrue:
		"Two-armed IFs forEffect share a single pop"
		[^super emitCodeForEffect: stack encoder: encoder].
	thenExpr := arguments at: 1.
	elseExpr := arguments at: 2.
	receiver emitCodeForValue: stack encoder: encoder.
	forValue
		ifTrue:  "Code all forValue as two-armed"
			[self emitCodeForBranchOn: false dist: thenSize pop: stack encoder: encoder.
			pc := encoder methodStreamPosition.
			thenExpr emitCodeForEvaluatedValue: stack encoder: encoder.
			stack pop: 1.  "then and else alternate; they don't accumulate"
			thenExpr returns not ifTrue:
				"...not ifTrue: avoids using ifFalse: alone during this compile)"
				"Elide jump over else after a return"
				[self emitCodeForJump: elseSize encoder: encoder].
			elseExpr emitCodeForEvaluatedValue: stack encoder: encoder]
		ifFalse:  "One arm is empty here (two-arms code forValue)"
			[thenSize > 0
				ifTrue:
					[self emitCodeForBranchOn: false dist: thenSize pop: stack encoder: encoder.
					pc := encoder methodStreamPosition.
					thenExpr emitCodeForEvaluatedEffect: stack encoder: encoder]
				ifFalse:
					[self emitCodeForBranchOn: true dist: elseSize pop: stack encoder: encoder.
					pc := encoder methodStreamPosition.
					elseExpr emitCodeForEvaluatedEffect: stack encoder: encoder]]