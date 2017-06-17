testing
referencesSameRemoteLocationAs: aRemote
	^ ((super referencesSameRemoteLocationAs: aRemote)
		or: [ self httpsUrl = aRemote httpsUrl ])
			