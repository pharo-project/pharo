tests
testNew
	[self assertOriginResolves: #home]
		on: ResolutionRequest
		do: [:req | req resume: self home].
	