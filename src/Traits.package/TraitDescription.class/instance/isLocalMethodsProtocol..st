testing
isLocalMethodsProtocol: aProtocol
	aProtocol methods ifEmpty: [ ^true ].
	
	^aProtocol methods anySatisfy: [ :each | self isLocalSelector: each]