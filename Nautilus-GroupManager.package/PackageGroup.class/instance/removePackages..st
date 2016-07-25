adding/removing
removePackages: aCollection
	packages removeAll: aCollection.
	GroupAnnouncer uniqueInstance announce: (AGroupContentHasBeenModified group: self)