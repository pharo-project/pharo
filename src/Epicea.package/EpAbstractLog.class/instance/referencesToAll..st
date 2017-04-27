accessing
referencesToAll: aCollectionOfEntries

	^ aCollectionOfEntries collect: [ :each | self referenceTo: each ]