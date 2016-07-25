tests
testModelPushDownVariableToMultipleClassesInSameHierarchy
	| class |
	model 
		defineClass: 'Object subclass: #SomeClass instanceVariableNames: '''' classVariableNames: ''Foo'' poolDictionaries: '''' category: #''Refactory-Test data'''.
	model 
		defineClass: 'SomeClass subclass: #Subclass instanceVariableNames: '''' classVariableNames: '''' poolDictionaries: '''' category: #''Refactory-Test data'''.
	class := model classNamed: #Subclass.
	class compile: 'foo ^Foo' classified: #(#accessing).
	model 
		defineClass: 'Subclass subclass: #AnotherSubclass instanceVariableNames: '''' classVariableNames: '''' poolDictionaries: '''' category: #''Refactory-Test data'''.
	(model metaclassNamed: #AnotherSubclass) compile: 'bar ^Foo'
		classified: #(#accessing).
	self executeRefactoring: (RBPushDownClassVariableRefactoring 
				model: model
				variable: #Foo
				class: class superclass).
	self deny: (class superclass directlyDefinesClassVariable: #Foo).
	self assert: (class directlyDefinesClassVariable: #Foo)