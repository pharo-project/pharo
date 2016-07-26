serialize/materialize
serializeInstancesStepWith: anEncoder

	objects do: [ :instance | self serializeInstance: instance with: anEncoder ]