accessing
ancestors
	"Ensure we load ancestors as appropriate if necessary. Failure to load force us to return an empty set to avoid errors, but we'll keep track of the fact that this failure may be transient and that we can continue."

	(ancestors anySatisfy: #isNil)
		ifTrue: [ self loadAncestorsAndStepChildren ].
	(ancestors anySatisfy: #isNil)
		ifTrue: [ ^ #() ].
	^ ancestors