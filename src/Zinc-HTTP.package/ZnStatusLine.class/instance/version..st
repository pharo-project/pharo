accessing
version: string
	version := ZnConstants knownHTTPVersions 
		detect: [ :each | each = string ] 
		ifNone: [ (ZnUnknownHttpVersion version: string) signal ] 