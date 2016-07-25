accessing
returnLast

	self returns
		ifFalse: 
			[returns := true.
			statements at: statements size put: statements last asReturnNode]