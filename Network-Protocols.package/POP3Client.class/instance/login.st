private protocol
login
	self loginMethod
		ifNil: [^self].
	self loginMethod == #clearText
		ifTrue: [^self clearTextLogin].
	self loginMethod == #APOP
		ifTrue: [^self apopLogin].
	(POP3LoginError protocolInstance: self) signal: 'Unsupported login procedure.'