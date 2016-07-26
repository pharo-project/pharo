replacing
replaceArgument: searchString with: replaceString when: aBlock 
	self addArgumentRule: (RBStringReplaceRule 
				searchFor: searchString
				replaceWith: replaceString
				when: aBlock)