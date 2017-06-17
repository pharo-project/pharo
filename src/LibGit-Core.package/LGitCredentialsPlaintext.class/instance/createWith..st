operations
createWith: aStructure
	<todo: 'use exceptions instead of assertions'>
	self assert: self username isEmptyOrNil not.
	self assert: self passphrase isEmptyOrNil not.

	self withReturnHandlerDo: [ 
		self 
			cred_userpass_plaintext_new: aStructure
			username: self username
			password: self passphrase ]