running
traverse: aTarjanNode
	| component |
	aTarjanNode tarjanIndex: runningIndex.
	aTarjanNode tarjanLowlink: runningIndex.
	runningIndex := runningIndex + 1.
	self putOnStack: aTarjanNode.
	self traverseChildrenOf: aTarjanNode.
	(self isRootNode: aTarjanNode)
		ifTrue: [ component := OrderedCollection new.	"start new SCC"
					self popStackAndUpdateCurrentScc: component until: aTarjanNode ]