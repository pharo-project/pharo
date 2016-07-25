running
checkMethod: aMethod 
	aMethod ast allDefinedVariables do: 
		[ :each | 
		each first isLowercase ifFalse: 
			[ result
				addMethod: aMethod;
				addSearchString: each ] ]