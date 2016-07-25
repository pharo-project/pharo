transforming
replaceArgument: sourceNode with: replacementNode 
	| rewriter |
	rewriter := RBParseTreeRewriter new.
	rewriter replaceTree: sourceNode withTree: replacementNode.
	(rewriter executeTree: inlineParseTree body) 
		ifTrue: [inlineParseTree body: rewriter tree]