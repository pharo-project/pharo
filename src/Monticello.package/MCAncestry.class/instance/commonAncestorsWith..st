ancestry
commonAncestorsWith: aVersionInfo

	| sharedAncestors mergedOrder sorter |
	sorter := MCVersionSorter new
						addVersionInfo: self;
						addVersionInfo: aVersionInfo.
	mergedOrder := sorter sortedVersionInfos.
	sharedAncestors := (sorter allAncestorsOf: self) intersection: (sorter allAncestorsOf: aVersionInfo).
	^ mergedOrder select: [:ea | sharedAncestors includes: ea]