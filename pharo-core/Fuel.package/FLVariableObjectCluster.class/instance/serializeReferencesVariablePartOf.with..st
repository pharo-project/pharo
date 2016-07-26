serialize/materialize
serializeReferencesVariablePartOf: anObject with: anEncoder
	
	(self variableReferences at: anObject ifAbsent: [ ^ self ]) do: [ :value |
		anEncoder encodeReferenceTo: value ]