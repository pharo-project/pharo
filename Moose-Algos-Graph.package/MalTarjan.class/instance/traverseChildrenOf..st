running
traverseChildrenOf: aTarjanNode
	aTarjanNode nextNodes
		do: [ :next | 
			next isTarjanUndefined
				ifTrue: [ self traverse: next.
							aTarjanNode tarjanLowlink: (aTarjanNode tarjanLowlink min: next tarjanLowlink) ]
				ifFalse: [ next inStack
								ifTrue: [ aTarjanNode tarjanLowlink: (aTarjanNode tarjanLowlink min: next tarjanIndex) ] ] ]