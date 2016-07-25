printing
printTestCase: aTestCase 
	self stderr 
		startOfLine; clearToEnd; green; 
		print: aTestCase; clear.
	self printProgress.