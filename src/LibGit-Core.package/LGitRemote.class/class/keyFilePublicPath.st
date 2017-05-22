accessing-credentials
keyFilePublicPath
	^ self credentials
		at: #keyFilePublicPath
		ifAbsentPut: [ UIManager default request: 'Path to your public key file (/home/foo/.ssh/key.pub)?' ]