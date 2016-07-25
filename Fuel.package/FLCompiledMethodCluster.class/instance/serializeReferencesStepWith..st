serialize/materialize
serializeReferencesStepWith: anEncoder

	objects do: [ :aCompiledMethod | self serializeLiteralsOf: aCompiledMethod with: anEncoder ]