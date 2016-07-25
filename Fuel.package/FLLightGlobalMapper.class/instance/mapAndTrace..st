mapping
mapAndTrace: anObject
	"Uses the Chain of Responsibility pattern to answer the cluster which maps with the received object."

	(globals includes: anObject) 
		ifTrue: [ self mapAndTraceByClusterName: anObject to: FLGlobalValueCluster ]
		ifFalse: [ next mapAndTrace: anObject ]