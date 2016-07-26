serialize/materialize
clusterMaterializeStepWith: aMaterialization

	super clusterMaterializeStepWith: aMaterialization.
	variablesMapping := aMaterialization decoder variablesMappingFor: theClass.