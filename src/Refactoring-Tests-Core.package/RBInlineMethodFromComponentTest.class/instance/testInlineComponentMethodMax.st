tests
testInlineComponentMethodMax
	| refactoring |
	self proceedThroughWarning: 
		[ | class |
		refactoring := RBInlineMethodFromComponentRefactoring 
			inline: (self 
					convertInterval: (47 to: 58)
					for: (RBRefactoryTestDataApp sourceCodeAt: #inlineMax))
			inMethod: #inlineMax
			forClass: RBRefactoryTestDataApp.
		self 
			setupInlineExpressionFor: refactoring
			toReturn: true.
		class := refactoring model classNamed: #Magnitude.
		class 
			compile: 'max: aMagnitude 
					"Answer the receiver or the argument, whichever has the greater magnitude."

					self > aMagnitude
						ifTrue: [^self]
						ifFalse: [^aMagnitude]'
			classified: #(#accessing ).
		self 
			setupImplementorToInlineFor: refactoring
			toReturn: class.
		self executeRefactoring: refactoring ].
	self assert: ((refactoring model classNamed: #RBRefactoryTestDataApp) parseTreeFor: #inlineMax) = (RBParser parseMethod: 'inlineMax
								| x y q |
								x := 5.
								y := 10.
								q := x + 1 > y
									ifTrue: [x + 1]
									ifFalse: [y].
								^q')