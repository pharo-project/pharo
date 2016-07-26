visiting
visitReturnNode: aReturnNode
	self addStyle: 	#return 
		  forNode: aReturnNode.
		
	self visitNode: aReturnNode value