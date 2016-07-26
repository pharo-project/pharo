copying
copyTraitExpression
	| newCopy |
	newCopy := self shallowCopy.
	newCopy transformations: (self transformations collect: [ :each | each copyTraitExpression ]).
	^ newCopy
