replacing
replaceTree: searchTree withTree: replaceTree when: aBlock 
	self addRule: (RBStringReplaceRule 
				searchForTree: searchTree
				replaceWith: replaceTree
				when: aBlock)