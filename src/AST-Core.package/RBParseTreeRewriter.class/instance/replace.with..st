replacing
replace: searchString with: replaceString 
	self addRule: (RBStringReplaceRule searchFor: searchString
				replaceWith: replaceString)