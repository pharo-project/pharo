actions
cleanUpAfterMorphicInitialization

	Author fullName: self class name.
	MCDataStream initialize.
	GradientFillStyle initPixelRampCache.
	FreeTypeCache clearCurrent.
	ImageMorph classPool at: #DefaultForm put: (Form extent: 1 @ 1 depth: 1).
	Behavior flushObsoleteSubclasses.
	World cleanseOtherworldlySteppers.
	Smalltalk organization removeEmptyCategories.
	MCFileBasedRepository flushAllCaches.
	MCMethodDefinition shutDown. 
	MCDefinition clearInstances.
	ExternalDropHandler resetRegisteredHandlers.
	Undeclared removeUnreferencedKeys.
	Smalltalk globals flushClassNameCache.
	ScrollBar initializeImagesCache.

	NaturalLanguageTranslator classPool at: #AllKnownPhrases put: nil.

	3 timesRepeat: [ 
		Smalltalk garbageCollect.
	 	Symbol compactSymbolTable ].
	 	HashedCollection rehashAll.
	  
	  "Remove empty categories, which are not in MC packages, because MC does
	  not do this (this script does not make packages dirty)"
	  Smalltalk organization removeEmptyCategories.
	  Smalltalk
	    allClassesAndTraitsDo: [ :class |
	      [ :each |
	        each
	          removeEmptyCategories;
	          sortCategories ]
	            value: class organization;
	            value: class class organization ].
	Smalltalk organization sortCategories.
	ChangeSet removeChangeSetsNamedSuchThat: [ :each | true ].
	ChangeSet resetCurrentToNewUnnamedChangeSet.
	Smalltalk garbageCollect.
	Author reset.