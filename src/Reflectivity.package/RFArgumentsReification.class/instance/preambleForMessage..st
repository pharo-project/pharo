generate
preambleForMessage: aNode
	| preamble arguments |
	preamble := OrderedCollection new.
	arguments := OrderedCollection new.

	(1 to: aNode numArgs) reverseWithIndexDo: [:each :index |  
			| name |
			name := 'RFArg', index asString, 'RFReification'.
			preamble add:  (RFStorePopIntoTempNode named: name).
	].
	
	(1 to: aNode numArgs) withIndexDo: [:each :index |  
			| name |
			name := 'RFArg', index asString, 'RFReification'.
			arguments add:  (RBTemporaryNode named: name).
	].
	preamble addAll: (RBArrayNode statements: arguments).
	preamble add: (RFStorePopIntoTempNode named: 'RFArgumentsReificationVar').
	preamble addAll: arguments.
	^ preamble 