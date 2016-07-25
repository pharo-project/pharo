instance creation
replaceLiteral: literal with: newLiteral 
	| rewriteRule |
	rewriteRule := self new.
	rewriteRule replaceTree: (RBLiteralNode value: literal)
		withTree: (RBLiteralNode value: newLiteral).
	^rewriteRule