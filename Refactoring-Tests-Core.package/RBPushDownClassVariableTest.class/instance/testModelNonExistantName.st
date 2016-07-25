failure tests
testModelNonExistantName
	model 
		defineClass: 'Object subclass: #SomeClass instanceVariableNames: '''' classVariableNames: '''' poolDictionaries: '''' category: #''Refactory-Test data'''.
	self shouldFail: (RBPushDownClassVariableRefactoring 
				model: model
				variable: #Foo
				class: (model classNamed: #SomeClass))