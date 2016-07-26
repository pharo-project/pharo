running
basicCheck: aMethod 
	^ (RBReadBeforeWrittenTester variablesReadBeforeWrittenIn: aMethod ast) notEmpty 