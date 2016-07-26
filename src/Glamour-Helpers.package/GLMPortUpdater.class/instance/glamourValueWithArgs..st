accessing
glamourValueWithArgs: anArray 
	 
	(anArray first pane port: self portSymbol) 
		value: (self valueBlock glamourValueWithArgs: anArray)