morphic ui
sortedVersions
	| sorter |
	sorter := MCVersionSorter new.
	sorter addAllVersionInfos: versions.
	^ sorter sortedVersionInfos select: [:ea | versions includes: ea]