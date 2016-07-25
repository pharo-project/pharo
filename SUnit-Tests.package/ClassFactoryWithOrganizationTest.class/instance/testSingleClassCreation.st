testing
testSingleClassCreation
	|class elementsInCategoryForTest |
	class := factory 
		newSubclassOf: Object 
		instanceVariableNames: 'a b c' 
		classVariableNames: 'X Y'.
	self assert: (self testedEnvironment allClasses includes: class).
	factory createdClasses do: [ :aClass |
		self assertEnvironmentOf: aClass ].
	elementsInCategoryForTest := self testedOrganization listAtCategoryNamed: factory defaultCategory. 
	self assert: elementsInCategoryForTest = {class name}.
	self assert: class instVarNames = #(a b c).
	self assert: class classPool keys asSet = #(X Y) asSet