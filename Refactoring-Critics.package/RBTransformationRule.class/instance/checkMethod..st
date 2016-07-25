running
checkMethod: aMethod 
	(self basicCheck: aMethod) ifTrue: [
		(self class recursiveSelfRule executeTree: rewriteRule tree initialAnswer: false) 
			ifFalse: [ self builder compile: rewriteRule tree newSource in: class classified: aMethod protocol ] ]