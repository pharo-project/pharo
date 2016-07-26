enumerating
allChangeSetNames
	^ self allChangeSets collect: [:c | c name]