replacing
replaceMethod: searchString with: replaceString 
	self addRule: (RBStringReplaceRule searchForMethod: searchString
				replaceWith: replaceString)