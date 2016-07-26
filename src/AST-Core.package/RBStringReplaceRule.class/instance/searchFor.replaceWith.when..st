initialize-release
searchFor: searchString replaceWith: replaceString when: aBlock 
	self searchFor: searchString replaceWith: replaceString.
	verificationBlock := aBlock