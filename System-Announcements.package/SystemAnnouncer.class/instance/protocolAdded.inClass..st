triggering
protocolAdded: aString inClass: aClass
	self announce: (ProtocolAdded new 
		classReorganized: aClass;
		protocol: aString;
		yourself)