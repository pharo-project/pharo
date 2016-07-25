settings
ft2LibraryVersion
	^ Smalltalk ui theme 
		newLabelIn: World 
		for: self 
		label: 'Available version: ', FT2Version current libraryVersion asString 
		getEnabled: nil.