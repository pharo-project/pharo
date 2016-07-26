protected-mapping
clusterKeyedByClusterName: aClusterClass factory: clusterFactory

	^ self
		clusterInstanceOf: aClusterClass
		keyInBucket: aClusterClass name
		factory: clusterFactory
