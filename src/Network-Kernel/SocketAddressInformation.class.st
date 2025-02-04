"
I represent a local or remote network service.

Instance Variables
	addressFamily:	<SmallInteger> the address family (unix, inet4, inet6, ...) in which the service address is available.
	protocol:		<SmallInteger> the protocol (tcp, udp, ...) that the service uses.
	socketAddress:	<SocketAddress> the socket address at which the service can be contacted or created.
	socketType:		<SmallInteger> the type (stream, dgram) of the socket that should be created for communication with the service.

"
Class {
	#name : 'SocketAddressInformation',
	#superclass : 'Object',
	#instVars : [
		'socketAddress',
		'addressFamily',
		'socketType',
		'protocol'
	],
	#classVars : [
		'AddressFamilyINET4',
		'AddressFamilyINET6',
		'AddressFamilyLocal',
		'AddressFamilyUnspecified',
		'NumericFlag',
		'PassiveFlag',
		'PrimitiveAccessProtect',
		'ProtocolTCP',
		'ProtocolUDP',
		'ProtocolUnspecified',
		'SocketTypeDGram',
		'SocketTypeStream',
		'SocketTypeUnspecified'
	],
	#category : 'Network-Kernel',
	#package : 'Network-Kernel'
}

{ #category : 'accessing' }
SocketAddressInformation class >> addressFamilyINET4 [

	^AddressFamilyINET4
]

{ #category : 'accessing' }
SocketAddressInformation class >> addressFamilyINET6 [

	^AddressFamilyINET6
]

{ #category : 'accessing' }
SocketAddressInformation class >> addressFamilyLocal [

	^AddressFamilyLocal
]

{ #category : 'accessing' }
SocketAddressInformation class >> addressFamilyUnspecified [

	^AddressFamilyUnspecified
]

{ #category : 'instance creation' }
SocketAddressInformation class >> forHost: hostName service: servName flags: flags addressFamily: family socketType: type protocol: protocol [

	| result addr |
	PrimitiveAccessProtect critical: [
		NetNameResolver initializeNetwork.
			NetNameResolver
				primGetAddressInfoHost: hostName
				service: servName
				flags: flags
				family: family
				type: type
				protocol: protocol.
			result := OrderedCollection new.
			[(addr := NetNameResolver nextSocketAddressInformation) notNil]
				whileTrue: [result add: addr]].
	^ result
]

{ #category : 'class initialization' }
SocketAddressInformation class >> initialize [
	"SocketAddressInformation initialize"

	NumericFlag := 1.
	PassiveFlag := 2.
	AddressFamilyUnspecified := 0.
	AddressFamilyLocal := 1.
	AddressFamilyINET4 := 2.
	AddressFamilyINET6 := 3.
	SocketTypeUnspecified := 0.
	SocketTypeStream := 1.
	SocketTypeDGram := 2.
	ProtocolUnspecified := 0.
	ProtocolTCP := 1.
	ProtocolUDP := 2.

	"SocketPlugin maintains internal state across primitive calls, so methods that rely
	on the result of sequential primitive calls require concurrency control."
	PrimitiveAccessProtect := Semaphore forMutualExclusion.

]

{ #category : 'accessing' }
SocketAddressInformation class >> numericFlag [

	^NumericFlag
]

{ #category : 'accessing' }
SocketAddressInformation class >> passiveFlag [

	^PassiveFlag
]

{ #category : 'accessing' }
SocketAddressInformation class >> protocolTCP [

	^ProtocolTCP
]

{ #category : 'accessing' }
SocketAddressInformation class >> protocolUDP [

	^ProtocolUDP
]

{ #category : 'accessing' }
SocketAddressInformation class >> protocolUnspecified [

	^ProtocolUnspecified
]

{ #category : 'accessing' }
SocketAddressInformation class >> socketTypeDGram [

	^SocketTypeDGram
]

{ #category : 'accessing' }
SocketAddressInformation class >> socketTypeStream [

	^SocketTypeStream
]

{ #category : 'accessing' }
SocketAddressInformation class >> socketTypeUnspecified [

	^SocketTypeUnspecified
]

{ #category : 'instance creation' }
SocketAddressInformation class >> withSocketAddress: socketAddress family: family type: type protocol: protocol [

	^self new initSocketAddress: socketAddress family: family type: type protocol: protocol
]

{ #category : 'accessing' }
SocketAddressInformation >> addressFamilyName [

	^#(unspecified local inet4 inet6) at: addressFamily + 1
]

{ #category : 'circuit setup' }
SocketAddressInformation >> connect [

	| sock |
	socketType == SocketTypeStream ifFalse: [^nil].
	sock := Socket newTCP: addressFamily.
	sock connectTo: socketAddress.
	sock waitForConnectionFor: Socket standardTimeout
		ifTimedOut: [ConnectionTimedOut signal: ('Cannot connect to {1}' translated format: {self})]
		ifRefused: [ConnectionRefused signal: ('Cannot connect to {1}' translated format: {self})].
	^sock
]

{ #category : 'initialization' }
SocketAddressInformation >> initSocketAddress: aSocketAddress family: familyInteger type: typeInteger protocol: protocolInteger [

	socketAddress := aSocketAddress.
	addressFamily := familyInteger.
	socketType := typeInteger.
	protocol := protocolInteger.
]

{ #category : 'circuit setup' }
SocketAddressInformation >> listenWithBacklog: backlog [

	| sock |
	(socketType == SocketTypeStream and: [protocol == ProtocolTCP]) ifFalse: [self error: 'cannot listen'].
	sock := Socket newTCP: addressFamily.
	sock bindTo: socketAddress.
	sock listenWithBacklog: 5.
	^sock
]

{ #category : 'printing' }
SocketAddressInformation >> printOn: aStream [

	aStream
		print: socketAddress;
		nextPut: $-; nextPutAll: self addressFamilyName;
		nextPut: $-; nextPutAll: self socketTypeName;
		nextPut: $-; nextPutAll: self protocolName
]

{ #category : 'accessing' }
SocketAddressInformation >> protocolName [

	^#(unspecified tcp udp) at: socketType + 1
]

{ #category : 'accessing' }
SocketAddressInformation >> socketAddress [

	^socketAddress
]

{ #category : 'accessing' }
SocketAddressInformation >> socketTypeName [

	^#(unspecified stream dgram) at: socketType + 1
]
