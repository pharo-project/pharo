visiting
visitPushInstVar: instVar
	
	stream nextPutAll: 'pushInstVar: '.
	instVar index printOn: stream.
