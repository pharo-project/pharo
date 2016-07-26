instance creation
compile: aString in: aBehavior classified: aProtocol for: aController
	^ self new class: aBehavior protocol: aProtocol source: aString controller: aController