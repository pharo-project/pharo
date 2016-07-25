tests
testModelRemoveUnusedVariable
	model 
		defineClass: 'Object subclass: #SomeClass instanceVariableNames: '''' classVariableNames: ''Foo'' poolDictionaries: '''' category: #''Refactory-Test data'''.
	self 
		assert: ((model classNamed: #SomeClass) directlyDefinesVariable: #Foo).
	model 
		defineClass: 'SomeClass subclass: #Subclass instanceVariableNames: '''' classVariableNames: '''' poolDictionaries: '''' category: #''Refactory-Test data'''.
	self executeRefactoring: (RBPushDownClassVariableRefactoring 
				model: model
				variable: #Foo
				class: (model classNamed: #SomeClass)).
	self deny: ((model classNamed: #SomeClass) directlyDefinesVariable: #Foo).
	self deny: ((model classNamed: #Subclass) directlyDefinesVariable: #Foo)