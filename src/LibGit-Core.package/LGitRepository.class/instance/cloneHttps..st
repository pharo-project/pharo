operations
cloneHttps: aString
	<todo: 'use exceptions instead of assertion'>
	self assert: (aString beginsWith: 'https://').
	
	self clone: aString