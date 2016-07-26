tests
testModelRenameSequenceClass
	model 
		defineClass: 'Object subclass: #Foo1 
			instanceVariableNames: '''' 
			classVariableNames: '''' poolDictionaries: ''''
			category: #''Refactory-Test data'''.
	(model classNamed: #Foo1)
		compile: 'foo	 ^ Foo1' classified: #(accessing);
		compile: 'objectName ^ #(Foo1)' classified: #(accessing).
	self executeRefactoring: (RBRenameClassRefactoring 
		model: model
		rename: (model classNamed: #Foo1)
		to: #Foo2).
	self executeRefactoring: (RBRenameClassRefactoring 
		model: model
		rename: (model classNamed: #Foo2)
		to: #Foo3).
	self deny: (model includesClassNamed: #Foo1).
	self deny: (model includesClassNamed: #Foo2).
	self assert: (model includesClassNamed: #Foo3).
	self assert: ((model classNamed: #Foo3) parseTreeFor: #foo) 
		= (RBParser parseMethod: 'foo ^ Foo3').
	self assert: ((model classNamed: #Foo3) parseTreeFor: #objectName) 
		= (RBParser parseMethod: 'objectName ^ #(Foo3)')