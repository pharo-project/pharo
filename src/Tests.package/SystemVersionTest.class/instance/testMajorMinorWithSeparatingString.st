tests
testMajorMinorWithSeparatingString 
	
	self assert: ((SystemVersion fromVersionString: 'Squeak3.7+alpha') majorMinor: '-') = '3-7'.
	self assert: ((SystemVersion fromVersionString: 'Pharo1.0+ALPHA') majorMinor: '')  = '10'.
	self assert: ((SystemVersion fromVersionString: 'Pharo1.1') majorMinor: '.') = '1.1'.