method-compiling
methodsFromDefinition: aBehaviorDefinition
	"Get all the methods we consider we want from the current environment and return a copy, binded to the new class"
	^ aBehaviorDefinition localMethods 
		collect: [ :m | self createNewMethod: m ].