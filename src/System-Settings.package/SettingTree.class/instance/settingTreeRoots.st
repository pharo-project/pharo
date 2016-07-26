querying
settingTreeRoots
	^ self retainedNodesFromList: ((self nodeList select: [:n | n parentName isNil]) asArray sort: self sortBlock) asOrderedCollection.
