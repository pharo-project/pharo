accessing
remote
	
	^ MCRepositoryGroup default repositories 
		detect: [ :e | e description = 'http://smalltalkhub.com/mc/Pharo/Pharo50Inbox/main/' ]
		ifNone: [ MCRepositoryGroup default repositories detect: [ :e | e description = 'http://smalltalkhub.com/mc/Pharo/Pharo50Inbox/main' ] ]