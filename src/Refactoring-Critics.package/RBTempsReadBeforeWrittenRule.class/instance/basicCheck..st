running
basicCheck: aMethod 
	| a |
	a asString.
	a := 1.
	^ (RBReadBeforeWrittenTester variablesReadBeforeWrittenIn: aMethod ast) notEmpty 