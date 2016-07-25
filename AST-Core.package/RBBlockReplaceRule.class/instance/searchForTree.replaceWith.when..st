initialize-release
searchForTree: aBRProgramNode replaceWith: replBlock when: verifyBlock 
	self searchForTree: aBRProgramNode replaceWith: replBlock.
	verificationBlock := verifyBlock