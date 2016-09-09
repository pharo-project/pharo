*Manifest-Core
removedRules
	^ {(RBCompositeLintRule new name: 'Spelling').  
		RBMissingTranslationsInMenusRule new.
		RBAssignmentWithoutEffectRule new.
		RBUtilityMethodsRule new.
		RBMethodHasNoTimeStampRule new. }
	
