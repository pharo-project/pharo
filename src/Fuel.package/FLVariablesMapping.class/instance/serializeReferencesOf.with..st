serialize/materialize
serializeReferencesOf: anObject with: anEncoder 

	(self references at: anObject ifAbsent: [ ^ self ]) do: [ :value |
		anEncoder encodeReferenceTo: value ].