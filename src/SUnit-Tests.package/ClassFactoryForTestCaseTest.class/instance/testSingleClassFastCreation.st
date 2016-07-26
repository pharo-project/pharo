testing
testSingleClassFastCreation
	|class elementsInCategoryForTest |
	class := factory newClass.
	self assert: (SystemNavigation new allClasses includes: class).
	elementsInCategoryForTest := SystemOrganization listAtCategoryNamed: factory defaultCategory. 
	self assert: elementsInCategoryForTest = {class name}.
	self assert: class instVarNames isEmpty.
	self assert: class classPool isEmpty