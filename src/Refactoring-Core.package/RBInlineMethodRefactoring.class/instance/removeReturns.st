transforming
removeReturns
	| rewriter |
	rewriter := RBParseTreeRewriter new.
	rewriter replace: '^``@object' with: '``@object'.
	(rewriter executeTree: inlineParseTree) 
		ifTrue: [inlineParseTree := rewriter tree]