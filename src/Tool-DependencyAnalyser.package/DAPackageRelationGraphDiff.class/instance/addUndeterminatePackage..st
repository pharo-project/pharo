adding
addUndeterminatePackage: aPackage
	^ packagesDiff
		add:
			((DAPackageItemDiff newWith: aPackage)
				beUnderteminate;
				diff: self);
				yourself