initializing
addVariables: aCollection ofType: aClass
	variables addAll: (aCollection collect: [:var | aClass name: var asString]).