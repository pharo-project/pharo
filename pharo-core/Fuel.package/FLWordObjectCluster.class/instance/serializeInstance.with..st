serialize/materialize
serializeInstance: aWordObject with: anEncoder

	anEncoder encodePositiveInteger: aWordObject basicSize.
	anEncoder encodeWords: aWordObject

