replacing
replace: searchString withValueFrom: replaceBlock 
	self addRule: (RBBlockReplaceRule searchFor: searchString
				replaceWith: replaceBlock)