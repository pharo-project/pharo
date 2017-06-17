private
setUpHandlers
	^ (self declaration associations collect: [ :nameToValue |
		nameToValue key -> (LGitCallReturnHandler for: (self perform: nameToValue key)) ]) asDictionary