adding
addNewDependentPackage: package from: parent
	^ dependentPackagesDiff
		add:
			((DADependentPackageItemDiff
				newWith: package)
				beNew;
				parent: parent;
				diff: self);
		yourself