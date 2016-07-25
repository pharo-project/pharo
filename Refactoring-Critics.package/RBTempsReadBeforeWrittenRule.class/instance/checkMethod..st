running
checkMethod: aMethod 
	(RBReadBeforeWrittenTester variablesReadBeforeWrittenIn: aMethod ast) do: 
		[ :each | 
		result 
			addMethod: aMethod;
			addSearchString: each ]