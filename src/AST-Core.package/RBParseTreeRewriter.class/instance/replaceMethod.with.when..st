replacing
replaceMethod: searchString with: replaceString when: aBlock 
	self addRule: (RBStringReplaceRule 
				searchForMethod: searchString
				replaceWith: replaceString
				when: aBlock)