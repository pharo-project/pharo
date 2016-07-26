code generation
sizeCodeForIf: encoder value: forValue
	| thenExpr elseExpr branchSize thenSize elseSize |
	thenExpr := arguments at: 1.
	elseExpr := arguments at: 2.
	(forValue
	 or: [(thenExpr isJust: NodeNil)
	 or: [elseExpr isJust: NodeNil]]) not
			"(...not ifTrue: avoids using ifFalse: alone during this compile)"
		ifTrue:  "Two-armed IFs forEffect share a single pop"
			[^super sizeCodeForEffect: encoder].
	forValue
		ifTrue:  "Code all forValue as two-armed"
			[elseSize := elseExpr sizeCodeForEvaluatedValue: encoder.
			thenSize := (thenExpr sizeCodeForEvaluatedValue: encoder)
					+ (thenExpr returns
						ifTrue: [0]  "Elide jump over else after a return"
						ifFalse: [self sizeCode: encoder forJump: elseSize]).
			branchSize := self sizeCode: encoder forBranchOn: false dist: thenSize]
		ifFalse:  "One arm is empty here (two-arms code forValue)"
			[(elseExpr isJust: NodeNil)
				ifTrue:
					[elseSize := 0.
					thenSize := thenExpr sizeCodeForEvaluatedEffect: encoder.
					branchSize := self sizeCode: encoder forBranchOn: false dist: thenSize]
				ifFalse:
					[thenSize := 0.
					elseSize := elseExpr sizeCodeForEvaluatedEffect: encoder.
					branchSize := self sizeCode: encoder forBranchOn: true dist: elseSize]].
	sizes := Array with: thenSize with: elseSize.
	^(receiver sizeCodeForValue: encoder)
	+ branchSize + thenSize + elseSize