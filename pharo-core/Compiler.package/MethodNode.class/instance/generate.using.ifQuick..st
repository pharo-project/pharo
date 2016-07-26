as yet unclassified
generate: trailer using: aCompiledMethodClass ifQuick: methodBlock
	| v |
	(primitive = 0 and: [arguments size = 0 and: [block isQuick]]) ifFalse:
		[^self].
	v := block code.
	v < 0 ifTrue:
		[^self].
	v = LdSelf ifTrue:
		[^methodBlock value: (aCompiledMethodClass toReturnSelfTrailerBytes: trailer)].
	(v between: LdTrue and: LdMinus1 + 3) ifTrue:
		[^methodBlock value: (aCompiledMethodClass toReturnConstant: v - LdSelf trailerBytes: trailer)].
	v < ((CodeBases at: LdInstType) + (CodeLimits at: LdInstType)) ifTrue:
		[^methodBlock value: (aCompiledMethodClass toReturnField: v trailerBytes: trailer)].
	v // 256 = 1 ifTrue:
		[^methodBlock value: (aCompiledMethodClass toReturnField: v \\ 256 trailerBytes: trailer)]