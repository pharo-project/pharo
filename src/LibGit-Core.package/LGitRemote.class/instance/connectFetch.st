accessing
connectFetch
	self withReturnHandlerDo: [ 
		self 
			remote_connect: self 
			direction: LGitDirectionEnum git_direction_fetch
			callbacks: LGitRemoteCallbacks defaults ]