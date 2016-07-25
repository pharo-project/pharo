initialize-release
searchFor: searchString replaceWith: aBlock 
	self searchString: searchString.
	replaceBlock := aBlock