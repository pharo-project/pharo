running
basicCheck: aMethod 
	^ (aMethod refersToLiteral: trueBinding) or: [ aMethod refersToLiteral: falseBinding ]