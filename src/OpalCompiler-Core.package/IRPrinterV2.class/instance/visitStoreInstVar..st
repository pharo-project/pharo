visiting
visitStoreInstVar: instVar
	stream nextPutAll: 'storeInstVar: '.
   instVar index printOn: stream.