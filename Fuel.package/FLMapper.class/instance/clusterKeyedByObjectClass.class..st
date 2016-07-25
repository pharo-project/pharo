protected-mapping
clusterKeyedByObjectClass: clusterClass class: objectClass

	^ self 
		clusterInstanceOf: clusterClass 
		keyInBucket: objectClass 
		factory: [ clusterClass newAnalyzing: objectClass ]