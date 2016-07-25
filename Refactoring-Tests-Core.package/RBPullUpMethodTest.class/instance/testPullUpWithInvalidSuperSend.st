failure tests
testPullUpWithInvalidSuperSend
	| class |
	model 
		defineClass: 'Object subclass: #SomeClass instanceVariableNames: '''' classVariableNames: ''Foo'' poolDictionaries: '''' category: #''Refactory-Test data'''.
	model 
		defineClass: 'SomeClass subclass: #Subclass instanceVariableNames: '''' classVariableNames: '''' poolDictionaries: '''' category: #''Refactory-Test data'''.
	model 
		defineClass: 'Subclass subclass: #Foo1 instanceVariableNames: '''' classVariableNames: '''' poolDictionaries: '''' category: #''Refactory-Test data'''.
	model 
		defineClass: 'Subclass subclass: #Foo2 instanceVariableNames: '''' classVariableNames: '''' poolDictionaries: '''' category: #''Refactory-Test data'''.
	(model classNamed: #Foo2) compile: 'yourself ^super yourself + 1'
		classified: #(#accessing).
	class := model classNamed: #Foo1.
	class compile: 'yourself ^1' classified: #(#accessing).
	self shouldFail: (RBPullUpMethodRefactoring 
				model: model
				pullUp: #(#yourself)
				from: class)