serialize/materialize
clusterMaterializeStepWith: aMaterialization

	super clusterMaterializeStepWith: aMaterialization.
	objects := Array new: aMaterialization decoder nextEncodedPositiveInteger