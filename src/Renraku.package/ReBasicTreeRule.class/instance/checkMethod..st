running
checkMethod: aMethod

	(self basicCheck: aMethod ast) ifTrue: 
		[ result addMethod: aMethod ]