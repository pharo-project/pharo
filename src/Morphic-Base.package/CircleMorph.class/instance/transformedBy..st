geometry
transformedBy: aTransform
	aTransform isIdentity ifTrue:[^self].
	^self center: (aTransform localPointToGlobal: self center).
