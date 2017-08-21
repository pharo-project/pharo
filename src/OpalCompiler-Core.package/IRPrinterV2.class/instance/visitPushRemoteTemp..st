visiting
visitPushRemoteTemp: remoteTemp

	stream nextPutAll: 'pushRemoteTemp: '.
	remoteTemp name printOn: stream.
	stream nextPutAll: ' inVector: '.
	remoteTemp tempVectorName printOn: stream