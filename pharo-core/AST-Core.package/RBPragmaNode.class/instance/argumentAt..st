accessing
argumentAt: keywordSelector
	^ self 
		argumentAt: keywordSelector
		ifAbsent: [ KeyNotFound signalFor: keywordSelector ]