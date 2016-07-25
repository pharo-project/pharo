testing
shouldVisitChildrenOf: anEntry
	^ selectChildren 
		ifNil: [ true ]
		ifNotNil: [ selectChildren cull: anEntry ]