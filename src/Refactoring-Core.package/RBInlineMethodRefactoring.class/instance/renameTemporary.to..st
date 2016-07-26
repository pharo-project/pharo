transforming
renameTemporary: oldName to: newName 
	| rewriter |
	rewriter := RBParseTreeRewriter new.
	rewriter
		replace: oldName with: newName;
		replaceArgument: oldName with: newName.
	(rewriter executeTree: inlineParseTree) 
		ifTrue: [inlineParseTree := rewriter tree]