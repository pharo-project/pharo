instance creation
searchFor: searchString replaceWith: replaceString when: aBlock 
	^self new 
		searchFor: searchString
		replaceWith: replaceString
		when: aBlock