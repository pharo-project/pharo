tests
testModelRemoveUnusedVariable
	model 
		defineClass: 'Object subclass: #SomeClass instanceVariableNames: ''foo'' classVariableNames: '''' poolDictionaries: '''' category: #''Refactory-Test data'''.
	model 
		defineClass: 'SomeClass subclass: #Subclass instanceVariableNames: '''' classVariableNames: '''' poolDictionaries: '''' category: #''Refactory-Test data'''.
	self executeRefactoring: (RBPushDownInstanceVariableRefactoring 
				model: model
				variable: 'foo'
				class: (model classNamed: #SomeClass)).
	self deny: ((model classNamed: #SomeClass) directlyDefinesVariable: 'foo').
	self assert: ((model classNamed: #Subclass) directlyDefinesVariable: 'foo')