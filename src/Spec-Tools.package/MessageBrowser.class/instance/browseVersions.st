actions
browseVersions
	self currentMethod ifNotNil: [ :method |
		model browseVersionsFrom: method compiledMethod ]