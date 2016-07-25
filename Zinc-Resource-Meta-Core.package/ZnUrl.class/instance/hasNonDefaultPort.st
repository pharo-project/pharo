testing
hasNonDefaultPort
	^ self hasPort 
		and: [ self port ~= (self class defaultPortForScheme: self schemeOrDefault) ]