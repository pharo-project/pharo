visiting
visitReturnNode: aReturnNode

	self addStyle: 	#return 
		startPosition: aReturnNode start 
		stopPosition: aReturnNode start. "the reason for start here is so not the whole return node is styled as bold"
	
	self visitNode: aReturnNode value