scripting
passivelyFrom: aPort
	^ self addPassiveOrigin: (self browser resolveOriginPort: aPort)