public
delete: path
	self 
		nodeAt: path parent
		ifPresent: [ :dict | 
			dict fileEntryRemove: path basename ifAbsent: [ FileDoesNotExist signalWith: path ]] 
		ifAbsent: [ DirectoryDoesNotExist signalWith: path parent ]