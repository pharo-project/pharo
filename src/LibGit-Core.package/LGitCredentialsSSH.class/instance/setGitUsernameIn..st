operations
setGitUsernameIn: aStructure
	self withReturnHandlerDo: [ 
		self cred_username_new: aStructure username: 'git'
	]