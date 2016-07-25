properties
removePropertiesIfEmpty
	^ Properties at: self ifPresent: [ :dict |
		dict ifEmpty: [ 
			method removeSavedTemp: self.
			Properties removeKey: self ] ]