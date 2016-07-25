running
basicCheck: aMethod 
	^ (aMethod ast isPrimitive not and: 
		[ matcher 
			executeMethod: aMethod ast
			initialAnswer: false ])