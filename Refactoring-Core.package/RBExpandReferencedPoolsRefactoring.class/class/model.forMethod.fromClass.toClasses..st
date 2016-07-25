instance creation
model: aRBNamespace forMethod: aParseTree fromClass: aClass toClasses: classCollection 
	^(self new)
		model: aRBNamespace;
		forMethod: aParseTree
			fromClass: aClass
			toClasses: classCollection;
		yourself