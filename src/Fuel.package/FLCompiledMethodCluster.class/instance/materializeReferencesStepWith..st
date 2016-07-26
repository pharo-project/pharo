serialize/materialize
materializeReferencesStepWith: aDecoder

	objects do: [ :aCompiledMethod | self materializeLiteralsTo: aCompiledMethod with: aDecoder ]