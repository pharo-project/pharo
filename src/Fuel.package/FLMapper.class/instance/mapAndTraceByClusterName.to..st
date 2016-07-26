protected-mapping
mapAndTraceByClusterName: anObject to: aClusterClass

	(self clusterKeyedByClusterName: aClusterClass)
		add: anObject traceWith: analysis
