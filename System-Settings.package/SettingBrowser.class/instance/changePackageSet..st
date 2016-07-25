updating
changePackageSet: aSet 
	aSet ~= self viewedPackages
		ifTrue: [self viewedPackages: aSet.
			self applyFilters].
	^ true