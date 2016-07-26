initialize-release
searchForMethod: searchString replaceWith: replaceString when: aBlock 
	self searchForMethod: searchString replaceWith: replaceString.
	verificationBlock := aBlock