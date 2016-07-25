private
newStore
	
	self flag: #todo. "The base locator could have a wrong fileSystem"
		
	^ OmFileStore 
		named: self newStoreName 
		inDirectory: baseLocator asFileReference ensureCreateDirectory 