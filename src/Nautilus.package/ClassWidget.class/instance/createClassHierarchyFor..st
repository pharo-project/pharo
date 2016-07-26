private
createClassHierarchyFor: aPackageGroup

	aPackageGroup ifNil: [ ^ {}].
	self cachedHierarchy: (self buildPackageGroupHierarchyFor: aPackageGroup asNautilusSelection).
	^ self cachedHierarchy sortedElements