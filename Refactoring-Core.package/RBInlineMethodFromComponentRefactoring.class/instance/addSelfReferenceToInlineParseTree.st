transforming
addSelfReferenceToInlineParseTree
	| variableName rewriter newArguments |
	variableName := self newNameForSelf.
	rewriter := RBParseTreeRewriter rename: 'self' to: variableName.
	(rewriter executeTree: inlineParseTree) 
		ifTrue: [inlineParseTree := rewriter tree].
	newArguments := inlineParseTree arguments asOrderedCollection.
	newArguments addFirst: (RBVariableNode named: variableName).
	inlineParseTree
		renameSelector: (self addArgumentToSelector: inlineParseTree selector)
		andArguments: newArguments.
	sourceMessage receiver replaceWith: (RBVariableNode named: variableName)