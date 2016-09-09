tests
testRecursiveMethod
	| class |
	model := RBClassModelFactory rbNamespace new.
	class := model classNamed: self class name.
	class
		compile: 'foo ^self foo' classified: #(#accessing);
		compile: 'bar ^self foo' classified: #(#accessing).
	self executeRefactoring: (RBInlineAllSendersRefactoring 
				model: model
				sendersOf: #foo
				in: class).
	self assert: (class parseTreeFor: #foo) 
				= (RBParser parseMethod: 'foo ^self foo').
	self assert: (class parseTreeFor: #bar) 
				= (RBParser parseMethod: 'bar ^self foo')