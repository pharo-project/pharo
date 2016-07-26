tests
testSecondsNanoSeconds 
	self assert: (Duration   seconds: 0 nanoSeconds: 5)  = (Duration  nanoSeconds: 5).	
	"not sure I should include in sunit since its Private "
	self assert: (aDuration seconds: 0 nanoSeconds: 1) = (Duration nanoSeconds: 1). 
