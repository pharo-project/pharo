transforming
rewriteInlinedTree
	sourceMessage parent isReturn 
		ifTrue: 
			[(sourceParseTree isLast: sourceMessage parent) 
				ifFalse: [self addSelfReturn]]
		ifFalse: 
			[self
				writeGuardClauses;
				normalizeIfTrues;
				normalizeReturns;
				addSelfReturn]