protected-mapping
mapAndTraceByObjectClass: anObject to: aClusterClass

	(self clusterKeyedByObjectClass: aClusterClass class: anObject class)
		add: anObject traceWith: analysis
