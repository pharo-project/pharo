adding/removing
addPackages: aCollection
	aCollection do: [ :each | packages addIfNotPresent: each ].
	GroupAnnouncer uniqueInstance announce: (AGroupContentHasBeenModified group: self)