*Manifest-Core
removedRules
	^ {(RBCompositeLintRule new name: 'Spelling').  
		RBMissingTranslationsInMenusRule new. 
		RBGuardingClauseRule new. 
		RBAssignmentWithoutEffectRule new.
		RBUtilityMethodsRule new.
		RBMethodHasNoTimeStampRule new. }
	
