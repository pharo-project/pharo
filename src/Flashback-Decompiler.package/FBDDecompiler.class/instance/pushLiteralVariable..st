data flow instructions
pushLiteralVariable: anAssociation
	simulatedStack addLast: (builder codeAnyLitInd: anAssociation)