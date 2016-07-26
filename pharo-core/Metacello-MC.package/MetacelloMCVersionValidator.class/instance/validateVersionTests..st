loading
validateVersionTests: version
	| testCases cleanTests |
	testCases := IdentitySet new.
	cleanTests := true.
	version currentlyLoadedClassesInVersion
		do: [ :class | 
			((class inheritsFrom: TestCase) and: [ class isAbstract not ])
				ifTrue: [ testCases add: class ] ].
	testCases
		do: [ :testCase | 
			| testResults |
			testResults := testCase suite run.
			testResults defects notEmpty
				ifTrue: [ 
					self
						recordValidationError:
							'Test failures in tests ' , testCase name asString , ' for ' , version versionString printString , ' in '
								, self configurationClass name asString , ' ' , testResults printString
						versionString: version versionString
						callSite: #validateVersionTests:
						reasonCode: #testFailures.
					cleanTests := false ] ].
	testCases isEmpty
		ifTrue: [ 
			self
				recordValidationCriticalWarning:
					'No test cases for ' , version versionString printString , ' in ' , self configurationClass name asString
				versionString: version versionString
				callSite: #validateVersionTests:
				reasonCode: #noTests.
			cleanTests := false ].
	^ cleanTests