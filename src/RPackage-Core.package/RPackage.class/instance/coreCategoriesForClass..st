system compatibility
coreCategoriesForClass: aClass

	self flag: #stef. "only used by goferCleanUp"
	^ aClass organization categories 
		reject: [ :cat | self isForeignClassExtension: cat ]