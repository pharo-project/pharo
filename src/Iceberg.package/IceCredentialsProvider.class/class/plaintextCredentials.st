accessing
plaintextCredentials
	^ PlaintextCredentials ifNil: [ PlaintextCredentials := IcePlaintextCredentials new ]