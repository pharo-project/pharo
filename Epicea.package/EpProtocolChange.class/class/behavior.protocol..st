instance creation
behavior: aBehavior protocol: aProtocol 
	^ self basicNew 
		initializeWithBehavior: aBehavior
		protocol: aProtocol;
		yourself 