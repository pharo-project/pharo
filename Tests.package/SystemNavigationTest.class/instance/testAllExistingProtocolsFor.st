testing
testAllExistingProtocolsFor

	| instSideProtocols classSideProtocols |

	instSideProtocols := self systemNavigationToTest allExistingProtocolsFor: true. 
	classSideProtocols := self systemNavigationToTest allExistingProtocolsFor: false.
	 
	self assert: (instSideProtocols allSatisfy: #isSymbol).	
	self assert: (classSideProtocols allSatisfy: #isSymbol).
	self assert: (instSideProtocols includesAll: (Object protocols)).
	self assert: (classSideProtocols includesAll: (Object class protocols)).