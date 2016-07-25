serialize/materialize
clusterSerializeStepWith: aSerialization

	super clusterSerializeStepWith: aSerialization.
	variablesMapping serializeOn: aSerialization encoder.