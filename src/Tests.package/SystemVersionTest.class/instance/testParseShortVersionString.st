as yet unclassified
testParseShortVersionString
	

	self assert: (SystemVersion parseShortVersionString: 'Squeak3.7alpha') = {'Squeak'. 3. 7. 'alpha'}.
	self assert: (SystemVersion parseShortVersionString: 'Pharo1.0ALPHA') = {'Pharo'. 1. 0. 'ALPHA'}.
	self assert: (SystemVersion parseShortVersionString: 'Pharo1.1') = {'Pharo'. 1. 1. ''}