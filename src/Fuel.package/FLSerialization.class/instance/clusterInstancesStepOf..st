private
clusterInstancesStepOf: aCluster

	encoder encodeClusterClass: aCluster class.
	aCluster clusterSerializeStepWith: self.
	aCluster serializeInstancesStepWith: encoder.
	aCluster serializePostInstancesStepWith: encoder.
