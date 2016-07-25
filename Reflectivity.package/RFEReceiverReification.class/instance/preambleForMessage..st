generate
preambleForMessage: aNode
	| preamble |
	preamble := OrderedCollection new.

	(1 to: aNode numArgs) reverseWithIndexDo: [:each :index |  
			| name |
			name := 'RFArg', index asString, 'RFReification'.
			preamble add:  (RFStorePopIntoTempNode named: name).
	].
	preamble 
		add: (RFStoreIntoTempNode named: #RFReceiverReificationVar).
		(1 to: aNode numArgs) withIndexDo: [:each :index |  
			| name |
			name := 'RFArg', index asString, 'RFReification'.
			preamble add:  (RBTemporaryNode named: name).
	].

	^ preamble