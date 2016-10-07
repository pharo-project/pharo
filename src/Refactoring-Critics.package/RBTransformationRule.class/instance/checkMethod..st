running
checkMethod: aMethod 
	(self basicCheck: aMethod) ifTrue: [
		self builder compile: rewriteRule tree newSource in: class classified: aMethod protocol ]