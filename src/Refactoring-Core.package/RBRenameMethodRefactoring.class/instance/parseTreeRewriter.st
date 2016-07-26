transforming
parseTreeRewriter
	| rewriteRule oldString newString |
	oldString := self buildSelectorString: oldSelector.
	newString := self buildSelectorString: newSelector
				withPermuteMap: permutation.
	rewriteRule := self hasPermutedArguments 
				ifTrue: [RBParseTreeRewriter new]
				ifFalse: [RBParseTreeRewriter replaceLiteral: oldSelector with: newSelector].
	rewriteRule replace: '``@object ' , oldString
		with: '``@object ' , newString.
	^rewriteRule