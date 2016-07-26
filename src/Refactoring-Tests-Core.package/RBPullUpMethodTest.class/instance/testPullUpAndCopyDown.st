tests
testPullUpAndCopyDown
	| class |
	model 
		defineClass: 'Object subclass: #SomeClass instanceVariableNames: '''' classVariableNames: ''Foo'' poolDictionaries: '''' category: #''Refactory-Test data'''.
	model 
		defineClass: 'SomeClass subclass: #Subclass instanceVariableNames: '''' classVariableNames: '''' poolDictionaries: '''' category: #''Refactory-Test data'''.
	model 
		defineClass: 'Subclass subclass: #Foo1 instanceVariableNames: '''' classVariableNames: '''' poolDictionaries: '''' category: #''Refactory-Test data'''.
	model 
		defineClass: 'Subclass subclass: #Foo2 instanceVariableNames: '''' classVariableNames: '''' poolDictionaries: '''' category: #''Refactory-Test data'''.
	class := model classNamed: #Foo1.
	class compile: 'yourself ^1' classified: #(#accessing).
	self executeRefactoring: (RBPullUpMethodRefactoring 
				model: model
				pullUp: #(#yourself)
				from: class).
	self assert: (class superclass parseTreeFor: #yourself) 
				= (RBParser parseMethod: 'yourself ^1').
	self deny: (class directlyDefinesMethod: #yourself).
	class := model classNamed: #Foo2.
	self assert: (class directlyDefinesMethod: #yourself).
	self assert: (class parseTreeFor: #yourself) 
				= ((model classNamed: #Object) parseTreeFor: #yourself)