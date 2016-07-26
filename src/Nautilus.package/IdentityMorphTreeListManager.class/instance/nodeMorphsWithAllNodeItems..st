accessing
nodeMorphsWithAllNodeItems: aNodeItemList

	^ self allNodeMorphs select: [:m | aNodeItemList identityIncludes: m complexContents withoutListWrapper]