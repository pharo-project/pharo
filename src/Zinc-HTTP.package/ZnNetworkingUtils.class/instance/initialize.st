initialization
initialize
	super initialize.
	self socketStreamClass: (Smalltalk globals at: #ZdcSocketStream ifAbsent: [ SocketStream ]).
	self secureSocketStreamClass: (Smalltalk globals at: #ZdcSecureSocketStream ifAbsent: [ nil ])