accessing-credentials
keyFilePrivatePath
	^ self credentials
		at: #keyFilePrivatePath
		ifAbsentPut: [ UIManager default request: 'Path to your private key file (/home/foo/.ssh/key)?' ]