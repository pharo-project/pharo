code generation
generate: trailer ifQuick: methodBlock
	| v |
	(primitive = 0 and: [arguments size = 0 and: [block isQuick]]) ifFalse:
		[^self].
	v := block code.
	v < 0 ifTrue:
		[^self].
	v = LdSelf ifTrue:
		[^methodBlock value: (CompiledMethod toReturnSelfTrailerBytes: trailer)].
	(v between: LdTrue and: LdMinus1 + 3) ifTrue:
		[^methodBlock value: (CompiledMethod toReturnConstant: v - LdSelf trailerBytes: trailer)].
	v < ((CodeBases at: LdInstType) + (CodeLimits at: LdInstType)) ifTrue:
		[^methodBlock value: (CompiledMethod toReturnField: v trailerBytes: trailer)].
	v // 256 = 1 ifTrue:
		[^methodBlock value: (CompiledMethod toReturnField: v \\ 256 trailerBytes: trailer)]