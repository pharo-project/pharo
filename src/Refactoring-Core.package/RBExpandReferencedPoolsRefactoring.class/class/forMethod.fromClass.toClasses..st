instance creation
forMethod: aParseTree fromClass: aClass toClasses: classCollection 
	^(self new)
		forMethod: aParseTree
			fromClass: aClass
			toClasses: classCollection;
		yourself