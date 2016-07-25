tests
testInlineComponentIntoCascadedMessage
	| refactoring |
	self proceedThroughWarning: 
		[ refactoring := RBInlineMethodFromComponentRefactoring 
			inline: (self 
					convertInterval: (35 to: 79)
					for: (RBRefactoryTestDataApp sourceCodeAt: #inlineComponent))
			inMethod: #inlineComponent
			forClass: RBRefactoryTestDataApp.
		(refactoring model classNamed: #Behavior) 
			compile: 'hasImmediateInstances ^self format = 0'
			classified: #(#accessing ).
		self 
			setupInlineExpressionFor: refactoring
			toReturn: false.
		self executeRefactoring: refactoring ].
	self assert: ((refactoring model classNamed: #RBRefactoryTestDataApp) parseTreeFor: #inlineComponent) = (RBParser parseMethod: 'inlineComponent
	| a aBehavior |
	a := 5.
	aBehavior := a class.
	aBehavior superclass.
	aBehavior format = 0.
	^aBehavior yourself')