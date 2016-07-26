adding
addUndeterminateDependentPackage: aPackage from: parent
	^ dependentPackagesDiff
		add:
				((DADependentPackageItemDiff newWith: aPackage)
						beUnderteminate;
						parent: parent;
						diff: self);
		yourself