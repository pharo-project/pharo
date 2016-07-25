replacing
replaceArgument: searchString with: replaceString 
	self addArgumentRule: (RBStringReplaceRule searchFor: searchString
				replaceWith: replaceString)