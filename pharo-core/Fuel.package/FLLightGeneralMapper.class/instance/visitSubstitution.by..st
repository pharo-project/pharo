visiting
visitSubstitution: anObject by: anotherObject

	(self clusterKeyedByClusterName: FLSubstitutionCluster)
		add: anObject substitutedBy: anotherObject traceWith: analysis
