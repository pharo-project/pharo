testing
testWithoutClassesRemovesClassesFromSender
	| environment newEnvironment |
	environment := (RBBrowserEnvironment new) forClasses: Number withAllSubclasses.
	
	newEnvironment := environment withoutClasses: { Number . Integer }.
	
	self 
		assert: newEnvironment classes size
		equals: environment classes size - 2