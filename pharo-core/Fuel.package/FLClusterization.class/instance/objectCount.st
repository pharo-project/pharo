accessing
objectCount

	^self clusterBuckets sum: [ :aClusterBucket | 
		aClusterBucket
			inject: 0 
			into: [ :sum :aCluster | sum + aCluster objects size ] ].