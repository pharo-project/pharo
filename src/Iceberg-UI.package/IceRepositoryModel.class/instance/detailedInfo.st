accessing
detailedInfo
	^ 	self repository origin detailedInfo, { 
		'Location' -> self repository location pathString. 
		'Pulling from' -> (self repository pullRemote 
			ifNotNil: [ :r | r remoteName ]
			ifNil: [ self repository origin remoteName ]). 
		'Pushing to' -> (self repository pushRemote 
			ifNotNil: [ :r | r remoteName ]
			ifNil: [ self repository origin remoteName ]).
		'On branch' -> self repository currentBranch name.
	} 
