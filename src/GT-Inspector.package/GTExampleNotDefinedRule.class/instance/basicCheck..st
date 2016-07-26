running
basicCheck: aClass 
	^ (self containsCustomExtension: aClass) 
		and: [ (self containsGTExample: aClass) not ]