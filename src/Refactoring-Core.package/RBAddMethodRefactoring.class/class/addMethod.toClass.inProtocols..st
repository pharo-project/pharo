instance creation
addMethod: aString toClass: aClass inProtocols: protocolList 
	^self new addMethod: aString
		toClass: aClass
		inProtocols: protocolList