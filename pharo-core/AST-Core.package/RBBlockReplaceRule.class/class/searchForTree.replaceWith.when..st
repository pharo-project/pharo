instance creation
searchForTree: aRBProgramNode replaceWith: replaceBlock when: aBlock 
	^self new 
		searchForTree: aRBProgramNode
		replaceWith: replaceBlock
		when: aBlock