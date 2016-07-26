serialize/materialize
materializeReferencesStepWith: aDecoder

	objects do: [ :anObject | self materializeReferencesOf: anObject with: aDecoder ]