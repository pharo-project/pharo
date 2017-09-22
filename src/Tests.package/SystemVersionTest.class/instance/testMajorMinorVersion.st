tests
testMajorMinorVersion 
	 	
	self assert: (SystemVersion fromVersionString: 'Squeak3.7+alpha') majorMinorVersion = 'Squeak3.7'.
	self assert: (SystemVersion fromVersionString: 'Pharo1.0+ALPHA') majorMinorVersion = 'Pharo1.0'.
	self assert: (SystemVersion fromVersionString: 'Pharo1.1') majorMinorVersion = 'Pharo1.1'.