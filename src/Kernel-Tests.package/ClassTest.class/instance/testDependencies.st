tests - dependencies
testDependencies
	self assert: (ClassTest dependentClasses includes: ClassTest superclass).
	self assert: (ClassTest dependentClasses includes: Date)