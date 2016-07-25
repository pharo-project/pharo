private
createMethodNamed: methodName inClass: aClass inCategory: aCategoryName 

	^ aClass compile: (methodName, ' ^nil') classified: aCategoryName.