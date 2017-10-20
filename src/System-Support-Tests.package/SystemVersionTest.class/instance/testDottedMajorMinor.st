tests
testDottedMajorMinor 
	 
	self assert: (SystemVersion fromVersionString: 'Squeak3.7+alpha') dottedMajorMinor = '3.7'.
	self assert: (SystemVersion fromVersionString: 'Pharo1.0+ALPHA') dottedMajorMinor = '1.0'.
	self assert: (SystemVersion fromVersionString: 'Pharo1.1') dottedMajorMinor = '1.1'.