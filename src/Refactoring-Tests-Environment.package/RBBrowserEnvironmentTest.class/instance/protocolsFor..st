private
protocolsFor: aBrowserEnvironment 
	aBrowserEnvironment classesAndSelectorsDo: [ :class :selector |
		| protocol |
		protocol := aBrowserEnvironment whichProtocolIncludes: selector in: class.
		self assert: (aBrowserEnvironment includesProtocol: protocol in: class) ]