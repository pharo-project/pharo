accessing
protocolNamed: aString 
	^ self protocolOrganizer 
		protocolNamed: aString 
		ifAbsent: [ nil ] 