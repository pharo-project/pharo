initialize-release
addMethod: aString toClass: aClass inProtocols: protocolList 
	class := self classObjectFor: aClass.
	source := aString.
	protocols := protocolList