tests
testDottedMajorMinor
	"
	SystemVersionTest run: #testMajorMinor
	"
	self assert: (SystemVersion fromShortVersionString: 'Squeak3.7alpha') dottedMajorMinor = '3.7'.
	self assert: (SystemVersion fromShortVersionString: 'Pharo1.0ALPHA') dottedMajorMinor = '1.0'.
	self assert: (SystemVersion fromShortVersionString: 'Pharo1.1') dottedMajorMinor = '1.1'.