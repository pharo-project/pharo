operations
createWith: aName onto: aStructure
	<todo: 'use exceptions instead of assertions'>
	self assert: aName isEmptyOrNil not.

	self withReturnHandlerDo: [ 
		self cred_ssh_key_new: aStructure username: aName ]