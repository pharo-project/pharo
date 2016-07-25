accessing
mczEntries
	^ (self directory fileNames select: [ :each | each endsWith: '.mcz' ])
		asSortedCollection