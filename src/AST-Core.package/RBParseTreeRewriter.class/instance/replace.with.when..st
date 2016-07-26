replacing
replace: searchString with: replaceString when: aBlock 
	self addRule: (RBStringReplaceRule 
				searchFor: searchString
				replaceWith: replaceString
				when: aBlock)