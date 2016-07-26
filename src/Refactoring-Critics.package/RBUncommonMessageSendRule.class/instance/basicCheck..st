running
basicCheck: aMethod 
	^ aMethod messages anySatisfy: [ :each |
		(each isEmpty or: [ each first isUppercase or: [ literalNames includes: each ] ]) ]