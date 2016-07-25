running
putOnStack: aTarjanNode
	stack push: aTarjanNode.
	aTarjanNode inStack: true. "I use a flag to avoid stack access"