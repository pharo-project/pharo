visiting
visitSmallInteger: aSmallInteger

	self mapAndTraceByClusterName: aSmallInteger to: (self clusterClassForSmallInteger: aSmallInteger)