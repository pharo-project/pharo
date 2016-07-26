visiting
visitSubstitution: anObject by: anotherObject onRecursionDo: aBlock

	| cluster |
	cluster := self clusterKeyedByClusterName: FLSubstitutionCluster.

	(cluster isSubstitute: anObject)
		ifTrue: aBlock
		ifFalse: [
			cluster
				add: anObject
				substitutedBy: anotherObject
				traceWith: analysis ]
