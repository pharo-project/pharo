replacing
replace: searchString withValueFrom: replaceBlock when: conditionBlock 
	self addRule: (RBBlockReplaceRule 
				searchFor: searchString
				replaceWith: replaceBlock
				when: conditionBlock)