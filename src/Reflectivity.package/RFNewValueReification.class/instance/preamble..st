generate
preamble: aNode
		link control= #instead 
		ifTrue: [ ^RFStorePopIntoTempNode named: #RFNewValueReificationVar  ]
		ifFalse: [^RFStoreIntoTempNode named: #RFNewValueReificationVar  ]
	
	