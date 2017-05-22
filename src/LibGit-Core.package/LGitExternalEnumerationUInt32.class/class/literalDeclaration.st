enum declaration
literalDeclaration
	| dictionary |
	dictionary := self enumDecl in: [ :decl |
		decl isDictionary
			ifTrue: [ decl ]
			ifFalse: [ Dictionary newFromPairs: decl ] ].
		
	self makeSelectorsLegalIn: dictionary.
	^ dictionary