operations
getEntryNamed: aString
	| entry |
	entry := self
		treebuilder_get: self
		filename: aString.
	entry repository: self repository.
	
	^ entry