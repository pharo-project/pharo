private
buildPackageGroupHierarchyFor: aPackageGroup
	^ self cachedHierarchy class buildHierarchyFor: aPackageGroup in: self model browsedEnvironment