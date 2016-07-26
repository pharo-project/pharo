initialize-release
searchForMethod: searchString replaceWith: replBlock when: verifyBlock 
	self searchForMethod: searchString replaceWith: replBlock.
	verificationBlock := verifyBlock