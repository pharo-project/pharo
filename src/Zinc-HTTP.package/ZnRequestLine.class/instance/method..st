accessing
method: string
	method := ZnConstants knownHTTPMethods 
		detect: [ :each | each = string ] 
		ifNone: [ (ZnUnknownHttpMethod method: string) signal ] 