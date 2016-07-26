running
checkMethod: aMethod
	aMethod isDoIt ifTrue: [ ^self ].
	(self basicCheck: aMethod) ifTrue: [
		self result addMethod: aMethod ]