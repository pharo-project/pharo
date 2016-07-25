tests - basic
testIsPrime

	"The following tests should return 'true'"
	self assert: 17 isPrime.
	self assert: 78901 isPrime.
	self assert: 104729 isPrime.
	self assert: 15485863 isPrime.
	self assert: 2038074743 isPrime.
	self assert: 29996224275833 isPrime.
	
	"The following tests should return 'false' (first 5 are Carmichael integers)"
	self deny: 561 isPrime.
	self deny: 2821 isPrime.
	self deny: 6601 isPrime.
	self deny: 10585 isPrime.
	self deny: 15841 isPrime.
	self deny: 256 isPrime.
	self deny: 29996224275831 isPrime.