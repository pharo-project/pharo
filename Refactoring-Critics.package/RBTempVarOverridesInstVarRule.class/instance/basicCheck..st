running
basicCheck: aMethod 
	vars := aMethod methodClass instVarNames.
	^ (matcher 
		executeTree: aMethod ast
		initialAnswer: false)