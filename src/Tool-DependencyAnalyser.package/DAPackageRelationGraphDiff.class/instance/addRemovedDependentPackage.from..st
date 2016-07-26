adding
addRemovedDependentPackage: package from: parent
	^ dependentPackagesDiff
		add:
			((DADependentPackageItemDiff
				newWith: package)
				beRemoved;
				parent: parent;
				diff: self);
		yourself