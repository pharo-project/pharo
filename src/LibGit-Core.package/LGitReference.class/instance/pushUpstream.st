accessing
pushUpstream
	| pushRemote | 
	^ self repository config 
		getString: 'remote.pushDefault'
		ifPresent: [ :pushRemoteName |
			self repository 
				lookupBranch: self upstream basename
				inRemote: pushRemoteName ]
		ifAbsent: [ self upstream ]
