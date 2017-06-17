operations
setUpstream: upstreamName
	self withReturnHandlerDo: [
		self branch_set_upstream: upstreamName
	]
