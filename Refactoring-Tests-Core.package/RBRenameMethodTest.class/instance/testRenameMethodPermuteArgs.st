tests
testRenameMethodPermuteArgs
	| refactoring class |
	refactoring := RBRenameMethodRefactoring
		renameMethod: ('demoRenameMethod:' , 'PermuteArgs:') asSymbol
		in: RBRefactoryTestDataApp
		to: ('demoRenameMethod:' , 'PermuteArgs:') asSymbol
		permutation: #(2 1).
	self executeRefactoring: refactoring.
	class := refactoring model classNamed: #RBRefactoryTestDataApp.
	self
		assert:
			(class parseTreeFor: ('demoRenameMethod:' , 'PermuteArgs:') asSymbol)
				=
					(RBParser
						parseMethod:
							'demoRenameMethod: arg2 PermuteArgs: arg1 self do: arg1.
	self do: arg2.
	^ arg1 > arg2').
	self
		assert:
			(class parseTreeFor: #demoExampleCall)
				= (RBParser parseMethod: 'demoExampleCall ^self demoRenameMethod: 2 PermuteArgs: 1')