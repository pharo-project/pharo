accessing
problemCount
	^environment isClassEnvironment 
		ifTrue: [self numberClasses]
		ifFalse: [super problemCount]