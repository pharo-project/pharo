system compatibility
extensionCategoriesForClass: aClass
	self flag: #stef. "only used by goferCleanUp and PackageEnvironment of RB"
	^ aClass organization categories 
		select: [ :cat | self isYourClassExtension: cat ]