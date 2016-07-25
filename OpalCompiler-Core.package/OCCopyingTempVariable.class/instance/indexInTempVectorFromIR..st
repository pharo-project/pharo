debugging
indexInTempVectorFromIR: aName
	^(scope methodScope node irInstruction tempVectorNamed: name) indexForVarNamed: aName.