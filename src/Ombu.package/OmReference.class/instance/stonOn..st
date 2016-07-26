ston persistence
stonOn: stonWriter
	
	stonWriter 
		writeObject: self 
		streamShortList: [ :array |
			array 
				add: self globalName; 
				add: self localName ]