preconditions
checkReferencesToSuperSendsToAnyOf: superMessages 
	[superMessages isEmpty] whileFalse: 
			[self 
				refactoringWarning: ('Although <1s> is equivalent to a superclass method,<n>it contains a super send so it might modify behavior.' 
						expandMacrosWith: superMessages first).
			superMessages remove: superMessages first]