private
obtainAvailableRemotes
	| remotes |
	
	remotes := self repository remotes.
	(remotes includes: self baseRemote)
		ifFalse: [ remotes := remotes copyWith: self baseRemote ].
	^ remotes	
