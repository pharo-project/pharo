printing
printReport
	| cr |
	cr := OSPlatform current lineEnding.
	
	self stderr
		<< cr
		<< 'Finished running ' << maxTest asString  
		<< ' Test' << (maxTest = 1 ifTrue: [''] ifFalse: ['s'])
		<< cr