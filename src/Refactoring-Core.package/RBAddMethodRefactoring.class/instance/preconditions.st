preconditions
preconditions
	| selector method |
	method := RBParser parseMethod: source
				onError: 
					[:string :position | 
					^RBCondition 
						withBlock: [self refactoringFailure: 'The sources could not be parsed']].
	selector := method selector.
	selector isNil ifTrue: [self refactoringFailure: 'Invalid source.'].
	^(RBCondition canUnderstand: selector in: class) not