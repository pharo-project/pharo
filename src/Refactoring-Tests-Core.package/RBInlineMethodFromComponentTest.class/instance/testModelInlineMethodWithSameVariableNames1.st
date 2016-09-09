tests
testModelInlineMethodWithSameVariableNames1
	| class refactoring |
	model := RBClassModelFactory rbNamespace new.
	class := model classNamed: #Rectangle.
	class 
		compile: 'rectangleRelativeTo: aRectangle ^self origin extent: aRectangle extent'
		classified: #(#accessing).
	(model classNamed: #Object) 
		compile: 'foo | aRectangle temp | aRectangle := 0@0 corner: 1@1. temp := aRectangle. ^aRectangle rectangleRelativeTo: temp'
		classified: #(#accessing).
	self proceedThroughWarning: 
			[refactoring := RBInlineMethodFromComponentRefactoring 
						model: model
						inline: (77 to: 112)
						inMethod: #foo
						forClass: (model classNamed: #Object).
			self setupInlineExpressionFor: refactoring toReturn: false.
			self setupImplementorToInlineFor: refactoring toReturn: class.
			self executeRefactoring: refactoring].
	self assert: ((refactoring model classNamed: #Object) parseTreeFor: #foo) 
				= (RBParser 
						parseMethod: 'foo | aRectangle temp | aRectangle := 0@0 corner: 1@1. temp := aRectangle. ^aRectangle origin extent: temp extent')