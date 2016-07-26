serialize/materialize
materializeReferencesOf: anObject with: aDecoder

	super materializeReferencesOf: anObject with: aDecoder.
	self materializeReferencesVariablePartOf: anObject with: aDecoder.