instance creation
searchFor: searchString replaceWith: replaceBlock when: aBlock 
	^self new 
		searchFor: searchString
		replaceWith: replaceBlock
		when: aBlock