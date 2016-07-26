accessing-dynamic
allFilesSorted
	^ self allFiles sorted: [ :a :b | 
		a modificationTime > b modificationTime ]