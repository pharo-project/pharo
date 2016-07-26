preconditions
preconditions
	^RBCondition withBlock: 
			[(class hierarchyDefinesInstanceVariable: variableName) 
				ifFalse: [self refactoringFailure: 'No subclass defines ' , variableName].
			(class subclasses 
				detect: [:each | (each directlyDefinesInstanceVariable: variableName) not]
				ifNone: [nil]) notNil 
				ifTrue: 
					[self 
						refactoringWarning: 'Not all subclasses have an instance variable named ' 
								, variableName , '.'].
			true]