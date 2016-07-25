serialize/materialize
registerIndexesOn: anEncoder
	substitutions keysAndValuesDo: [ :anObject :theSubstitution | 
		| substitutionIndex |
		substitutionIndex := anEncoder objectsIndexes at: theSubstitution.
		anEncoder substitutionIndexes
			at: anObject
			put: substitutionIndex ]