private
doUnvisitedEdges: aBlock

	self edges do: [:e|
		e cseVisited ifFalse: [
			aBlock value: e
		]
	]
