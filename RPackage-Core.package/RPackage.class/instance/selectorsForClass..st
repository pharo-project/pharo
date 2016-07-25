accessing
selectorsForClass: aClass
	"Returns all selectors for a class in the receiver without distinction of extensions or not"
	
	^ (self includesClass: aClass)
		ifFalse: [self extensionSelectorsForClass: aClass]
		ifTrue: [self definedSelectorsForClass: aClass]
	