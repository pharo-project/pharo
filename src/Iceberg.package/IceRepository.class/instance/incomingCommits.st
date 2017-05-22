querying
incomingCommits
	self pullRemote ifNil: [ ^ #() ].
	^ self incomingCommitsFrom: self pullRemote