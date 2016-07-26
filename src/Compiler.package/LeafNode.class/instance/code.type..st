private
code: argIndex type: type

	argIndex ifNil: [^type negated].
	(CodeLimits at: type) > argIndex 
		ifTrue: [^(CodeBases at: type) + argIndex].
	^type * 256 + argIndex