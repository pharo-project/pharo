public
allGTExamplesTo: examples
	Object withAllSubclasses do: [ :class | 
		class gtExamplesFinder ifNotNil: [ :finder | 
			finder gtExamplesContainedTo: examples ] ]