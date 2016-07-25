instance creation
searchForTree: aRBProgramNode replaceWith: replaceString when: aBlock 
	^self new 
		searchForTree: aRBProgramNode
		replaceWith: replaceString
		when: aBlock