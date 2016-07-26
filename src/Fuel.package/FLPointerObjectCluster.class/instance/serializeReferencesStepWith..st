serialize/materialize
serializeReferencesStepWith: anEncoder

	objects do: [ :anObject | self serializeReferencesOf: anObject with: anEncoder ]