utilities
associationForClassVariable: aName in: aClass ifAbsent: aBlock 
	^aClass realClass classPool associationAt: aName asSymbol
		ifAbsent: [aClass realClass classPool associationAt: aName asString ifAbsent: aBlock]