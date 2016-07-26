tests
testModelRenameClass
	| refactoring class |
	model 
		defineClass: 'Object subclass: #Foo instanceVariableNames: ''a'' classVariableNames: '''' poolDictionaries: '''' category: #''Refactory-Test data'''.
	class := model classNamed: #Foo.
	class
		compile: 'foo	^Object' classified: #(#accessing);
		compile: 'objectName ^#(Object)' classified: #(#accessing).
	refactoring := RBRenameClassRefactoring 
				model: model
				rename: Object
				to: #Thing.
	self executeRefactoring: refactoring.
	self assert: (model includesClassNamed: #Thing).
	self deny: (model includesClassNamed: #Object).
	self 
		assert: (class parseTreeFor: #foo) = (RBParser parseMethod: 'foo ^Thing').
	self assert: (class parseTreeFor: #objectName) 
				= (RBParser parseMethod: 'objectName ^#(Thing)').
	self assert: class superclass name = #Thing