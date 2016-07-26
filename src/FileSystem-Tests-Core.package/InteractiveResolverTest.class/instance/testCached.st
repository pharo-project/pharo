tests
testCached
	[ resolver resolve: #home ]
		on: ResolutionRequest
		do: [ :req | req resume: self home ].
	self assertOriginResolves: #home