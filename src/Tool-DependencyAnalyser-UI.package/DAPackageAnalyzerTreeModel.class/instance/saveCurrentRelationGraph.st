saving
saveCurrentRelationGraph
	self createDirectoryForSaving.
	^  FLSerializer serialize: relationGraph toFileNamed: self defaultName. 