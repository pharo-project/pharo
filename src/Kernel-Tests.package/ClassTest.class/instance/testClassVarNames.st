tests - class variables
testClassVarNames

	self assert: (Object classVarNames includes: #DependentsFields).
	
	"A class and it's meta-class share the class variables"
	self assert: (Object classVarNames = Object class classVarNames).