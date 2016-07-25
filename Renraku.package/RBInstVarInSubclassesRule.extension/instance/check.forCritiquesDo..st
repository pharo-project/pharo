*renraku
check: aClass forCritiquesDo: aCriticBlock
 
	| subs sels |
	subs := aClass subclasses.
	subs size > 1 ifFalse: [ ^ self ].

	sels := subs flatCollect: #instVarNames as: Bag.
	
	sels asSet
		select: [ :sel | 
			(sels occurrencesOf: sel) == subs size ]
		thenDo: [ :sel | aCriticBlock cull: ((ReRefactoringCritique
			withAnchor: (ReSearchStringSourceAnchor
				entity: aClass
				string: sel)
			by: self
			hint: sel)
			refactoring: (
				RBPullUpInstanceVariableRefactoring
					variable: sel
					class: aClass);
			yourself) ]