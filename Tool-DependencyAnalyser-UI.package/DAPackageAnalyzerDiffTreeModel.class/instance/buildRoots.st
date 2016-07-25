initialization
buildRoots
	self treeDiff roots: packageRelationGraphDiff packagesDiffToDisplay.
	self treeDiff whenBuiltDo: [ treeDiff rootNodeHolder: [ :item | DAPackageItemDiffNode new content: item ] ]