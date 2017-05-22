operations
createWith: aStructure
	<todo: 'use exceptions instead of assertions'>
	self assert: self username isEmptyOrNil not.
	self assert: self publicKeyReference exists.
	self assert: self privateKeyReference exists.

	self withReturnHandlerDo: [ 
		self 
			cred_ssh_key_new: aStructure
			username: self username
			publickey: self publicKeyReference fullName
			privatekey: self privateKeyReference fullName
			passphrase: self passphrase ]