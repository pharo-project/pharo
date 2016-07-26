accessing
whichProtocolIncludes: aSelector in: aClass
	^aClass organization categoryOfElement: aSelector ifAbsent: [ Protocol unclassified ]