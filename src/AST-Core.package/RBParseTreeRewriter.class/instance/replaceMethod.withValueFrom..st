replacing
replaceMethod: searchString withValueFrom: replaceBlock 
	self addRule: (RBBlockReplaceRule searchForMethod: searchString
				replaceWith: replaceBlock)