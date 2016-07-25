instance creation
methodRemoved: aCompiledMethod protocol: aProtocol origin: aBehavior
	^self new
		method: aCompiledMethod;
		protocol: aProtocol;
		methodOrigin: aBehavior;
		yourself