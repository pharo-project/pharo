tests
testMoveWithoutSelfReference
	| refactoring class selectorsSize |
	refactoring := RBExtractMethodToComponentRefactoring 
				extract: (self convertInterval: (118 to: 286)
						for: (RBReadBeforeWrittenTester sourceCodeAt: #copyDictionary:))
				from: #copyDictionary:
				in: RBReadBeforeWrittenTester.
	self setupMethodNameFor: refactoring toReturn: #copyWithAssociations.
	self setupVariableToMoveToFor: refactoring toReturn: 'aDictionary'.
	self setupVariableTypesFor: refactoring
		toReturn: (Array with: (refactoring model classNamed: #Dictionary)).
	class := refactoring model classNamed: #RBReadBeforeWrittenTester.
	selectorsSize := class selectors size.
	self executeRefactoring: refactoring.
	self 
		assert: (class parseTreeFor: #copyDictionary:) = (RBParser 
						parseMethod: 'copyDictionary: aDictionary ^aDictionary copyWithAssociations').
	self 
		assert: ((refactoring model classNamed: #Dictionary) 
				parseTreeFor: #copyWithAssociations) = (RBParser 
							parseMethod: 'copyWithAssociations 
							| newDictionary |
							newDictionary := Dictionary new: self size.
							self
								keysAndValuesDo: [:key :value | newDictionary at: key put: value].
							^newDictionary').
	self assert: class selectors size = selectorsSize