tests
testInlineEmptyComponentMethod
	| refactoring |
	self proceedThroughWarning: 
		[ refactoring := RBInlineMethodFromComponentRefactoring 
			inline: (self 
					convertInterval: (35 to: 91)
					for: (RBRefactoryTestDataApp sourceCodeAt: #inlineComponent))
			inMethod: #inlineComponent
			forClass: RBRefactoryTestDataApp.
		self 
			setupInlineExpressionFor: refactoring
			toReturn: false.
		"The following line is needed because some people implement #yourself themselves."
		self 
			setupImplementorToInlineFor: refactoring
			toReturn: (refactoring model classNamed: #Object).
		self executeRefactoring: refactoring ].
	self assert: ((refactoring model classNamed: #RBRefactoryTestDataApp) parseTreeFor: #inlineComponent) = (RBParser parseMethod: 'inlineComponent
	| a anObject |
	a := 5.
	anObject := a class.
	anObject superclass.
	anObject hasImmediateInstances.
	^anObject')