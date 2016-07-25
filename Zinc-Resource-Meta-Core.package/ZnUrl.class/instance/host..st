accessing
host: hostName
	host := hostName ifNil: [ nil ] ifNotNil: [ hostName asLowercase ]