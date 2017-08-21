visiting
visitStoreRemoteTemp: remoteTemp
	stream nextPutAll: 'storeRemoteTemp: '.
	remoteTemp name printOn: stream.
	stream nextPutAll: ' inVector: '.
	remoteTemp tempVectorName printOn: stream.