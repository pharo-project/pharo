operations
hasEntry: aString
	^ self withReturnHandlerDo: [ 
		self
			index_find: nil
			index: self
			path: aString ]