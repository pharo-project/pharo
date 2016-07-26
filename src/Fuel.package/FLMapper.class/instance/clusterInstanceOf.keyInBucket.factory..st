protected-mapping
clusterInstanceOf: aClusterClass keyInBucket: clusterKey factory: clusterFactory

	| bucket |
	bucket := aClusterClass clusterBucketIn: analysis clusterization.
	^ bucket
		at: clusterKey
		ifAbsentPut: [ 
			clusterFactory value 
				traceWith: analysis;
				yourself ].
