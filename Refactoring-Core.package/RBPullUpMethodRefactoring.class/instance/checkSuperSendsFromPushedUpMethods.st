preconditions
checkSuperSendsFromPushedUpMethods
	selectors do: 
			[:each | 
			| parseTree |
			parseTree := class parseTreeFor: each.
			(parseTree superMessages 
				detect: [:sup | class superclass directlyDefinesMethod: sup]
				ifNone: [nil]) notNil 
				ifTrue: 
					[self 
						refactoringError: ('Cannot pull up <1s> since it sends a super message that is defined in the superclass.' 
								expandMacrosWith: each)]]