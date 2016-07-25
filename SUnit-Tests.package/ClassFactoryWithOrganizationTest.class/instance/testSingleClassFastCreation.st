testing
testSingleClassFastCreation
	|class elementsInCategoryForTest |
	class := factory newClass.
	self assert: (self testedEnvironment allClasses includes: class).
	elementsInCategoryForTest := self testedOrganization listAtCategoryNamed: factory defaultCategory. 
	factory createdClasses do: [ :aClass |
		self assertEnvironmentOf: aClass ].
	self assert: elementsInCategoryForTest = {class name}.
	self assert: class instVarNames isEmpty.
	self assert: class classPool isEmpty