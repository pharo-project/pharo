instance creation
model: aRBSmalltalk addMethod: aString toClass: aClass inProtocols: protocolList 
	^(self new)
		model: aRBSmalltalk;
		addMethod: aString
			toClass: aClass
			inProtocols: protocolList;
		yourself