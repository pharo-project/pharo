failure tests
testPullUpWithSuperSendThatCannotBeCopiedDown
	| class |
	model 
		defineClass: 'Object subclass: #SomeClass instanceVariableNames: '''' classVariableNames: ''Foo'' poolDictionaries: '''' category: #''Refactory-Test data'''.
	(model classNamed: #Object) compile: 'foo ^3' classified: #(#accessing).
	model 
		defineClass: 'SomeClass subclass: #Subclass instanceVariableNames: '''' classVariableNames: '''' poolDictionaries: '''' category: #''Refactory-Test data'''.
	(model classNamed: #Subclass) compile: 'foo ^super foo'
		classified: #(#accessing).
	model 
		defineClass: 'Subclass subclass: #Foo1 instanceVariableNames: '''' classVariableNames: '''' poolDictionaries: '''' category: #''Refactory-Test data'''.
	model 
		defineClass: 'Subclass subclass: #Foo2 instanceVariableNames: '''' classVariableNames: '''' poolDictionaries: '''' category: #''Refactory-Test data'''.
	(class := model classNamed: #Foo2) compile: 'foo ^1'
		classified: #(#accessing).
	self shouldFail: (RBPullUpMethodRefactoring 
				model: model
				pullUp: #(#foo)
				from: class)