transforming
abstractInstanceVariable: aString 
	| refactoring rewriter |
	refactoring := RBCreateAccessorsForVariableRefactoring 
				model: self model
				variable: aString
				class: fromClass
				classVariable: false.
	self performComponentRefactoring: refactoring.
	rewriter := RBParseTreeRewriter new.
	rewriter
		replace: aString , ' := ``@object'
			with: ('self <1s> ``@object' expandMacrosWith: refactoring setterMethod);
		replace: aString with: 'self ' , refactoring getterMethod.
	(rewriter executeTree: tree) ifTrue: [tree := rewriter tree]