code generation
emitCodeForEvaluatedValue: stack encoder: encoder
	| position |
	position := stack position.
	self emitCodeExceptLast: stack encoder: encoder.
	statements last emitCodeForBlockValue: stack encoder: encoder.
	[stack position - 1 = position] assert