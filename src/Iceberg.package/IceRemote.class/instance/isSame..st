testing
isSame: aRemote
	^ (self referencesSameRemoteLocationAs: aRemote)
		and: [ self remoteName = aRemote remoteName ]