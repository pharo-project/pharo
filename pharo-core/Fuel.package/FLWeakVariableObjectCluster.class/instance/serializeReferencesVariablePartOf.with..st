serialize/materialize
serializeReferencesVariablePartOf: anObject with: anEncoder
	
	1 to: anObject basicSize do: [ :index |
		anEncoder encodeWeakReferenceTo: (anObject basicAt: index) ]