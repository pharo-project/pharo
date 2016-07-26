tests
testModelPushDownVariableToClassDownTwoLevels
	| class |
	model defineClass: 'Object subclass: #SomeClass instanceVariableNames: '''' classVariableNames: ''Foo'' poolDictionaries: '''' category: #''Refactory-Test data'''.
	model defineClass: 'SomeClass subclass: #Subclass instanceVariableNames: '''' classVariableNames: '''' poolDictionaries: '''' category: #''Refactory-Test data'''.
	model defineClass: 'Subclass subclass: #AnotherSubclass instanceVariableNames: '''' classVariableNames: '''' poolDictionaries: '''' category: #''Refactory-Test data'''.
	class := model metaclassNamed: #AnotherSubclass.
	class 
		compile: 'bar ^Foo'
		classified: #(#accessing ).
	class := class theNonMetaClass.
	self executeRefactoring: (RBPushDownClassVariableRefactoring 
			model: model
			variable: #Foo
			class: class superclass superclass).
	self deny: (class superclass superclass directlyDefinesClassVariable: #Foo).
	self deny: (class superclass directlyDefinesClassVariable: #Foo).
	self assert: (class directlyDefinesClassVariable: #Foo)