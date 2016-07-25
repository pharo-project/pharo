tests
testModelAddMethod
	| refactoring class |
	class := model metaclassNamed: #Bar.
	refactoring := RBAddMethodRefactoring 
				model: model
				addMethod: 'printString1 ^super printString'
				toClass: class
				inProtocols: #(#accessing).
	self executeRefactoring: refactoring.
	self assert: (class parseTreeFor: #printString1) 
				= (RBParser parseMethod: 'printString1 ^super printString')