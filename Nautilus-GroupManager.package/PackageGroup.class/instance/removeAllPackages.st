adding/removing
removeAllPackages
	packages removeAll.
	GroupAnnouncer uniqueInstance announce: (AGroupContentHasBeenModified group: self)