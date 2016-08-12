tests - to be refined
testRewriteDynamicArray
	| newSource |
	rewriter := RBParseTreeRewriter new replace: '
		{`@first. `@second. `@third}' with: 'Array with: `@first  with: `@second  with: `@third'.

	newSource := (rewriter executeTree: (RBParser parseRewriteExpression: ' {(1 @ 255).	(Color lightMagenta). 3}'))
		ifTrue: [ rewriter tree formattedCode].
	self assert: newSource = 'Array with: 1 @ 255 with: Color lightMagenta with: 3'.