method tests
testDefinesPoolDictionary
	self deny: (objectClass definesPoolDictionary: #OpcodePool).
	self assert: (newClass definesPoolDictionary: #TextConstants).
	self deny: (messageNodeClass definesPoolDictionary: #OpcodePool).
	self assert: ((RBClassModelFactory rbNamespace new classNamed: #Text) 
				definesPoolDictionary: #TextConstants)