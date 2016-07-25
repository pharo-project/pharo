system integration
packageForProtocol: aProtocolName inClass: aClass
	"According aProtocolName is an extension protocol, will look for a matching package. If no matching package is found ,  return nil. If aProtocolName is not an extension protocol, return the parent package of aClass"
	
	|tmpProtocol|
	tmpProtocol := aProtocolName ifNil: [''].
	^ (tmpProtocol beginsWith: '*')
		ifTrue: [  (self packageMatchingExtensionName: (tmpProtocol copyWithout: $*)) ]
		ifFalse: [ aClass package ]