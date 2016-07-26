ancestry
allAncestorsOnPathTo: aVersionInfo
	^ MCFilteredVersionSorter new
		target: aVersionInfo;
		addAllVersionInfos: self ancestors;
		sortedVersionInfos