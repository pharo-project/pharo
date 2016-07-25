serialize/materialize
serializeReferencesOf: anObject with: anEncoder

	super serializeReferencesOf: anObject with: anEncoder.
	self serializeReferencesVariablePartOf: anObject with: anEncoder. 