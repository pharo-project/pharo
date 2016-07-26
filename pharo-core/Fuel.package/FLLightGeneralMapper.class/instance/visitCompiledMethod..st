visiting
visitCompiledMethod: aCompiledMethod
	(aCompiledMethod isInstalled not or: [aCompiledMethod isDoIt ])
		ifTrue: [ self mapAndTraceByClusterName: aCompiledMethod to: FLCompiledMethodCluster ]
		ifFalse: [self mapAndTraceByClusterName: aCompiledMethod to: FLGlobalCompiledMethodCluster]
		
	
	