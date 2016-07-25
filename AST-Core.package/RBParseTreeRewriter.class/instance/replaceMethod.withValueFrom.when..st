replacing
replaceMethod: searchString withValueFrom: replaceBlock when: conditionBlock 
	self addRule: (RBBlockReplaceRule 
				searchForMethod: searchString
				replaceWith: replaceBlock
				when: conditionBlock)