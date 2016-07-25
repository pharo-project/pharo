protocol
rootsForStagingArea: aStagingArea

	^ aStagingArea packages 
		collect: [ :each | self buildRootForPackage: each ]
		thenSelect: [:each | each isEmpty not ]