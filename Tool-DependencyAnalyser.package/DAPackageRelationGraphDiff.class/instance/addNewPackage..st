adding
addNewPackage: seenPackage
	^ packagesDiff
		add:
			((DAPackageItemDiff newWith: seenPackage)
				beNew;
				diff: self);
		yourself