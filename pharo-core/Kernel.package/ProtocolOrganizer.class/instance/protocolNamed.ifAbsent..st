accessing
protocolNamed: aString ifAbsent: aBlock 
	^  self allProtocols 
		detect: [ :e | e name = aString ] 
		ifNone: aBlock