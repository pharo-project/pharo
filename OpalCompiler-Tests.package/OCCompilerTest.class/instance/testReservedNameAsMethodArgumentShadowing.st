test shadowing
testReservedNameAsMethodArgumentShadowing

	interactive := true.
	#( 'self' 'super' 'thisContext' 'true' 'false' 'nil' ) do: [ :each |
		self initializeErrorMessage.
		[ :exit | 
			OpalCompiler new 
				source: 'temp: ' , each , ' ^ ' , each;
				class:  MockForCompilation;
				requestor: self;
				failBlock: [ exit value ];
				translate.
			self fail ] valueWithExit.
		self assert: ((errorMessage = 'Variable name expected ->' )or: [ errorMessage =  'Name already defined ->' ]).
		self assert: errorLocation = 7 ]