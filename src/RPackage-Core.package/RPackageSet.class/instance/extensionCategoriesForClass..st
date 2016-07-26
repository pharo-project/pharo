system compatibility
extensionCategoriesForClass: aClass
	^ aClass organization categories select: [:cat | self isYourClassExtension: cat]