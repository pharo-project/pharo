mapping
mapAndTrace: anObject
	"Uses the Chain of Responsibility pattern to answer the cluster which maps with the received object."

	(condition value: anObject)
		ifTrue: [ self mapAndTraceSubstitutionIfNotRecursive: anObject ]
		ifFalse: [ next mapAndTrace: anObject ]