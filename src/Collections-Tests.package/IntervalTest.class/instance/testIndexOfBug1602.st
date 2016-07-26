tests
testIndexOfBug1602
	"This test is by german morales.
	It is about mantis bug 1602"
	
	self should: ((1 to: 5 by: 1) indexOf: 2.5) = 0. "obvious"
	self should: ((100000000000000 to: 500000000000000 by: 100000000000000)
 		  indexOf: 250000000000000) = 0. "same as above with 14 zeros appended"