actions
addMethodsUnderTestIn: listOfPackages to: methods 
	listOfPackages
		do: [:package | package isNil
				ifFalse: [package methodReferences
						do: [:method | ((#(#packageNamesUnderTest #classNamesNotUnderTest ) includes: method selector)
									or: [method compiledMethod isAbstract
											or: [method compiledMethod refersToLiteral: #ignoreForCoverage]])
								ifFalse: [methods add: method]]]]