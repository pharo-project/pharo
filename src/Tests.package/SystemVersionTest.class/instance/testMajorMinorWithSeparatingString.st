tests
testMajorMinorWithSeparatingString
	"SystemVersionTest debug: #testMajorMinorWithSeparatingString"
	
	self assert: ((SystemVersion fromShortVersionString: 'Squeak3.7alpha') majorMinor: '-') = '3-7'.
	self assert: ((SystemVersion fromShortVersionString: 'Pharo1.0ALPHA') majorMinor: '')  = '10'.
	self assert: ((SystemVersion fromShortVersionString: 'Pharo1.1') majorMinor: '.') = '1.1'.