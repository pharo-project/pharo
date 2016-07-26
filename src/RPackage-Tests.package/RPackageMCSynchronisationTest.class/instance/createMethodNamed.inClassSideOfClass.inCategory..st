private
createMethodNamed: methodName inClassSideOfClass: aClass inCategory: aCategoryName 

	^ aClass classSide compile: (methodName, ' ^nil') classified: aCategoryName.