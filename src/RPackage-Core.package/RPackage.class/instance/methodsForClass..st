accessing
methodsForClass: aClass
	"Returns all compiled methods for a class in the receiver without distinction of extensions or not"
	
	^ (self includesClass: aClass)
		ifFalse: [self extensionMethodsForClass: aClass]
		ifTrue: [self definedMethodsForClass: aClass]
	