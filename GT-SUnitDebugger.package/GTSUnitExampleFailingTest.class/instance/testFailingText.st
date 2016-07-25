tests
testFailingText

	self assert: 'I like pie  
pizza pie 
apple pie 
rad squared pi 
and other PIes'
	equals: 'I like pie  
pizza pie 
potato pie 
rad squared pi 
and other PIes'