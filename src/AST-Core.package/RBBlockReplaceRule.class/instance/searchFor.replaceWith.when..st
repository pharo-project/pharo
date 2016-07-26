initialize-release
searchFor: searchString replaceWith: replBlock when: verifyBlock 
	self searchFor: searchString replaceWith: replBlock.
	verificationBlock := verifyBlock