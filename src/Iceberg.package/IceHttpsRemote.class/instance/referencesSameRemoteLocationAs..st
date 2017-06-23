testing
referencesSameRemoteLocationAs: aRemote
	^ (super referencesSameRemoteLocationAs: aRemote)
		or: [ self url = aRemote httpsUrl ] 