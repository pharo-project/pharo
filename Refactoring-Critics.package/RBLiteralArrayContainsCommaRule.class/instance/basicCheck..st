running
basicCheck: aMethod 
	^ aMethod allLiterals anySatisfy: [ :each | self doesLiteralArrayContainComma: each ]