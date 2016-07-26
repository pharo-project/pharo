tests
testInclusionBug1603
	"This test is by german morales.
	It is about mantis bug 1603"
	
	self shouldnt: ((1 to: 5 by: 1) includes: 2.5). "obvious"
	self shouldnt: ((100000000000000 to: 500000000000000 by: 100000000000000)
 		  includes: 250000000000000). "same as above with 14 zeros appended"