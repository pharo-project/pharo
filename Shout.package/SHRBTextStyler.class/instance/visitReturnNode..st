visiting rb nodes
visitReturnNode: aReturnNode 
	self 
		addStyle: #return
		from: aReturnNode start
		to: aReturnNode stop.
		
	self visitNode: aReturnNode value