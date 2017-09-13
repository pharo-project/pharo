running
check: aClass forCritiquesDo: aCritiqueBlock
 
	| subs sels |
	subs := aClass subclasses.
	subs size > 1 ifFalse: [ ^ self ].

	sels := subs flatCollect: #instVarNames as: Bag.
	
	sels asSet
		select: [ :sel | 
			(sels occurrencesOf: sel) == subs size ]
		thenDo: [ :sel | aCritiqueBlock cull: (
			(self critiqueFor: aClass)
				tinyHint: sel;
				refactoring: (
					RBPullUpInstanceVariableRefactoring
						variable: sel
						class: aClass);
				yourself) ]