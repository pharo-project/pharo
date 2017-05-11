ston persistence
stonOn: stonWriter
	
	stonWriter 
		writeObject: self 
		streamShortList: [ :array |
			"Optimization: only write globalName if it belongs to a 'remote' store"
			OmCurrentGlobalName value = self globalName
				ifFalse: [ array add: self globalName ].

			array add: self localName ]