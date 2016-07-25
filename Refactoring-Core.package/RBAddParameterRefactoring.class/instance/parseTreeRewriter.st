private
parseTreeRewriter
	| rewriteRule oldString newString |
	rewriteRule := RBParseTreeRewriter new.
	oldString := self buildSelectorString: oldSelector.
	newString := self newSelectorString.
	rewriteRule replace: '``@object ' , oldString
		with: '``@object ' , newString.
	^rewriteRule