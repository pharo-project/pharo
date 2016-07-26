serialize/materialize
clusterMaterializeStepWith: aMaterialization

	super clusterMaterializeStepWith: aMaterialization.
	theClass := aMaterialization decoder nextEncodedReference.
