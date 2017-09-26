tests
testMajorMinorVersion
	"SystemVersionTest run: #testMajorMinorVersion"
	
	self assert: (SystemVersion fromShortVersionString: 'Squeak3.7alpha') majorMinorVersion = 'Squeak3.7'.
	self assert: (SystemVersion fromShortVersionString: 'Pharo1.0ALPHA') majorMinorVersion = 'Pharo1.0'.
	self assert: (SystemVersion fromShortVersionString: 'Pharo1.1') majorMinorVersion = 'Pharo1.1'.