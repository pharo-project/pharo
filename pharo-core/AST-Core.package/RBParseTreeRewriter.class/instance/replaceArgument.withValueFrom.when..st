replacing
replaceArgument: searchString withValueFrom: replaceBlock when: conditionBlock 
	self addArgumentRule: (RBBlockReplaceRule 
				searchFor: searchString
				replaceWith: replaceBlock
				when: conditionBlock)