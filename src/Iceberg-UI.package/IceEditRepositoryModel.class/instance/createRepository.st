actions
createRepository
	self repository location = self location 
		ifFalse: [ self repository location: self location ].
	self repository subdirectory = self codeDirectory
		ifFalse: [ self repository subdirectory: self codeDirectory ].
	self window delete. 
	acceptBlock ifNotNil: [ acceptBlock cull: self ]