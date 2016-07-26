preconditions
preconditions
	"This refactoring only preserves behavior if all implementors are renamed."

	| conditions |
	conditions := self myConditions 
				& (RBCondition definesSelector: oldSelector in: class) 
				& (RBCondition isValidMethodName: newSelector for: class).
					
	conditions := self implementors 
		inject: conditions into: [ :condition :each | 
			condition & (RBCondition hierarchyOf: each canUnderstand: newSelector) not ].
		
	^conditions & (RBCondition withBlock: 
		[self implementors size > 1 
			ifTrue: 
				[self refactoringWarning: ('This will modify all <1p> implementors.' 
							expandMacrosWith: self implementors size)].
		true])