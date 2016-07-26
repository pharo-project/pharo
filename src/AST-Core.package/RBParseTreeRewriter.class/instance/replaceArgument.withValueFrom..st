replacing
replaceArgument: searchString withValueFrom: replaceBlock 
	self addArgumentRule: (RBBlockReplaceRule searchFor: searchString
				replaceWith: replaceBlock)