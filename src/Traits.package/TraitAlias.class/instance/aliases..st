accessing
aliases: anArrayOfAssociations
	| newNames |
	newNames := (anArrayOfAssociations collect: [:each | each key]) asIdentitySet.
	newNames size < anArrayOfAssociations size ifTrue: [
		TraitCompositionException signal: 'Cannot use the same alias name twice'].
	anArrayOfAssociations do: [:each |
		(newNames includes: each value) ifTrue: [
			TraitCompositionException signal: 'Cannot define an alias for an alias']].
	aliases := anArrayOfAssociations