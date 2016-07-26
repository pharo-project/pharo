actions
seenPackagesWithFilter: aFilter
	^ self packages 
		select: [ :each | each isSeen and: [ aFilter match: each packageName ] ].