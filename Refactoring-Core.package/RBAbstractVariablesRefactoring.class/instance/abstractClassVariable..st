transforming
abstractClassVariable: aString 
	| refactoring rewriter nonMetaClass |
	nonMetaClass := fromClass theNonMetaClass.
	refactoring := RBCreateAccessorsForVariableRefactoring 
		model: self model
		variable: aString
		class: nonMetaClass
		classVariable: true.
	self performComponentRefactoring: refactoring.
	rewriter := RBParseTreeRewriter new.
	fromClass isMeta 
		ifTrue: 
			[ rewriter
				replace: aString , ' := ``@object'
					with: ('self <1s> ``@object' expandMacrosWith: refactoring setterMethod);
				replace: aString
					with: 'self ' , refactoring getterMethod ]
		ifFalse: 
			[ rewriter
				replace: aString , ' := ``@object'
					with: ('self class <1s> ``@object' expandMacrosWith: refactoring setterMethod);
				replace: aString
					with: 'self class ' , refactoring getterMethod ].
	(rewriter executeTree: tree) ifTrue: [ tree := rewriter tree ]