serialize/materialize
clusterSerializeStepWith: aSerialization

	super clusterSerializeStepWith: aSerialization.
	aSerialization encoder encodePositiveInteger: objects size.