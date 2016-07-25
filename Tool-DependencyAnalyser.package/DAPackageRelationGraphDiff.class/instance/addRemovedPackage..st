adding
addRemovedPackage: seenPackage
	^ packagesDiff
		add:
			((DAPackageItemDiff newWith: seenPackage)
				beRemoved;
				diff: self);
		yourself