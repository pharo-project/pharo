commits
incomingCommitsFrom: aRemote 
	^ (self upstreamFor: aRemote) 
		ifNotNil: [ :remoteUpstream | remoteUpstream commitsNotIn: self ]
		ifNil: [ #() ]