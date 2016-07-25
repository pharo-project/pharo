tests
testModelInlineRecursiveMethod
	| refactoring class |
	class := model classNamed: #Object.
	class compile: 'foo self bar. self foo. self bar' classified: #(#accessing).
	refactoring := RBInlineMethodRefactoring 
				model: model
				inline: (15 to: 23)
				inMethod: #foo
				forClass: class.
	self executeRefactoring: refactoring.
	self 
		assert: (class parseTreeFor: #foo) = (RBParser 
						parseMethod: 'foo self bar. self bar. self foo. self bar. self bar')