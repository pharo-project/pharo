api
setRefreshingBlockForImplementorsOf: aSelector

	self refreshingBlock: [:message | message selector = aSelector ].