testing
isEmpty
	^ preMaterializationActions isEmpty 
		and: [ postMaterializationActions isEmpty
		and: [ additionalObjects isEmpty ] ]