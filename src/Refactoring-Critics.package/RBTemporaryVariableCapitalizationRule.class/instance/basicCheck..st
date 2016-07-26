running
basicCheck: aMethod 
	^ aMethod ast allDefinedVariables anySatisfy:  
		[ :each | 
		each first isLowercase not ] 