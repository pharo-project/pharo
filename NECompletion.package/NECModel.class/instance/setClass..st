initialize-release
setClass: aClass 
	clazz := aClass.
	self initializeSelectors.
	self narrowWith: String new