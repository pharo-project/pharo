mapping
mapAndTraceSubstitutionIfNotRecursive: anObject
		
	| cluster |
	cluster := self clusterKeyedByClusterName: FLSubstitutionCluster.

	(cluster isSubstitute: anObject)
		ifTrue: [ next mapAndTrace: anObject ]
		ifFalse: [
			cluster
				add: anObject
				substitutedBy: (substitutionFactory value: anObject)
				traceWith: analysis ]
