instance creation
buildHierarchyFor: aPackageGroup in: anEnvironment
	^ aPackageGroup withClassesIn: anEnvironment createHierarchySorter: self 