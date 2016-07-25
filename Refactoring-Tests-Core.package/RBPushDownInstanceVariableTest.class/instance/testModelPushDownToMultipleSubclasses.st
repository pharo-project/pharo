tests
testModelPushDownToMultipleSubclasses
	model 
		defineClass: 'Object subclass: #SomeClass instanceVariableNames: ''foo'' classVariableNames: '''' poolDictionaries: '''' category: #''Refactory-Test data'''.
	model 
		defineClass: 'SomeClass subclass: #Subclass instanceVariableNames: '''' classVariableNames: '''' poolDictionaries: '''' category: #''Refactory-Test data'''.
	(model classNamed: #Subclass) compile: 'foo ^foo' classified: #(#accessing).
	model 
		defineClass: 'SomeClass subclass: #AnotherSubclass instanceVariableNames: '''' classVariableNames: '''' poolDictionaries: '''' category: #''Refactory-Test data'''.
	(model classNamed: #AnotherSubclass) compile: 'bar ^foo'
		classified: #(#accessing).
	self executeRefactoring: (RBPushDownInstanceVariableRefactoring 
				model: model
				variable: 'foo'
				class: (model classNamed: #SomeClass)).
	self deny: ((model classNamed: #SomeClass) directlyDefinesVariable: 'foo').
	self 
		assert: ((model classNamed: #Subclass) directlyDefinesVariable: 'foo').
	self assert: ((model classNamed: #AnotherSubclass) 
				directlyDefinesVariable: 'foo')