triggering
protocolRemoved: aString inClass: aClass
	self announce: (ProtocolRemoved new 
		classReorganized: aClass;
		protocol: aString;
		yourself)