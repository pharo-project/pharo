replacing
replaceTree: searchTree withTree: replaceTree 
	self addRule: (RBStringReplaceRule searchForTree: searchTree
				replaceWith: replaceTree)