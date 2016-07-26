code generation
sizeCodeForCase: encoder value: forValue

	| braceNode sizeIndex elseSize allReturn |
	forValue not ifTrue:
		[^super sizeCodeForEffect: encoder].
	equalNode := encoder encodeSelector: #=.
	braceNode := arguments first.
	sizes := Array new: 2 * braceNode numElements.
	sizeIndex := sizes size.
	elseSize := arguments size = 2
		ifTrue:
			[arguments last sizeCodeForEvaluatedValue: encoder] "otherwise: [...]"
		ifFalse:
			[caseErrorNode := encoder encodeSelector: #caseError.
			 (NodeSelf sizeCodeForValue: encoder)
			 + (caseErrorNode sizeCode: encoder args: 0 super: false)]. "self caseError"
	"There must be at least one branch around the otherwise/caseError
	  so the decompiler can identify the end of the otherwise/caseError."
	allReturn := true. "assume every case ends with a return"
	braceNode casesForwardDo:
		[:keyNode :valueNode :last |
		valueNode returns ifFalse: [allReturn := false]].
	braceNode casesReverseDo:
		[:keyNode :valueNode :last | | thenSize |
		sizes at: sizeIndex put: elseSize.
		thenSize := valueNode sizeCodeForEvaluatedValue: encoder.
		last ifFalse: [thenSize := thenSize + encoder sizePop].
		valueNode returns ifFalse: [thenSize := thenSize + (self sizeCode: encoder forJump: elseSize)].
		(last and: [allReturn]) ifTrue: [thenSize := thenSize + (self sizeCode: encoder forJump: elseSize)].
		sizes at: sizeIndex-1 put: thenSize.
		last ifFalse: [elseSize := elseSize + encoder sizeDup].
		elseSize := elseSize
					+ (keyNode sizeCodeForEvaluatedValue: encoder)
					+ (equalNode sizeCode: encoder args: 1 super: false)
					+ (self sizeCode: encoder forBranchOn: false dist: thenSize)
					+ thenSize.
		sizeIndex := sizeIndex - 2].
	^(receiver sizeCodeForValue: encoder) + elseSize