instance creation
searchForMethod: searchString replaceWith: replaceString when: aBlock 
	^self new 
		searchForMethod: searchString
		replaceWith: replaceString
		when: aBlock