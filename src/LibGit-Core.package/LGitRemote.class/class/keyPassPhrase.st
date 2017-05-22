accessing-credentials
keyPassPhrase
	^ self credentials
		at: #keyPassPhrase
		ifAbsentPut: [ UIManager default request: 'Passphrase of your private key file?' ]