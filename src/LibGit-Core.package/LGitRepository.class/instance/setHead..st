operations
setHead: aLGitReference
	self withReturnHandlerDo: [ 
		self
			repository_set_head: aLGitReference name
			log_message: 'set head to refs/heads/' , aLGitReference name ]