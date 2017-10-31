accessing
newButtonRow

	^ self toolbar 
		ifNil: [ nil ]
		ifNotNil: [ : t | t buildWithSpec ]