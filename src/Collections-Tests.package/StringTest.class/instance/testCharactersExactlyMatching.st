tests
testCharactersExactlyMatching
	self assert: 5 equals: ('voiture' charactersExactlyMatching: 'voituze').
	self assert: 0 equals: ('voiture' charactersExactlyMatching: '').
	self assert: 1 equals: ('voiture' charactersExactlyMatching: 'v').
	self assert: 0 equals: ('voiture' charactersExactlyMatching: 'abcdef').