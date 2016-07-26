public
methodRefs
	| theClass classVarAssoc |
	theClass := receiverClass classThatDefinesClassVariable: variableName.
	classVarAssoc := theClass classPool associationAt: variableName asSymbol.
	classVarAssoc value 
		ifNil: [ ^ SystemNavigation new allCallsOn: classVarAssoc ]
		ifNotNil: [ ^ classVarAssoc value class ]