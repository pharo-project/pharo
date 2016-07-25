enumeration
keysAndValuesDo: aBlock withAssociationString: associationString
	| equalsIndex |
	equalsIndex := associationString indexOf: $=.
	aBlock 
		value: (associationString first: equalsIndex-1) 
		value: (associationString allButFirst: equalsIndex).
		