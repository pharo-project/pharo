composition
@ anArrayOfAssociations 
	^TraitAlias 
		with: subject
		aliases: (anArrayOfAssociations, self aliases)