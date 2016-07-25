preconditions
preconditions
	^ classNames
		inject: RBCondition empty
		into: [ :sum :each | 
			| aClassOrTrait |
			aClassOrTrait := self model classNamed: each asSymbol.
			aClassOrTrait isNil
				ifTrue: [ self refactoringFailure: 'No such class or trait' ].
			sum
				& ( (self preconditionIsNotMetaclass: aClassOrTrait) 
					& (self preconditionHasNoReferences: each)
					& (self preconditionHasNoSubclasses: aClassOrTrait)
					& (self preconditionHasNoUsers: aClassOrTrait ) ) ]