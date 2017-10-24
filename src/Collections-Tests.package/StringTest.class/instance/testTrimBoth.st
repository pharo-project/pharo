tests - converting
testTrimBoth
	self assert: '' trimBoth = ''.
	self assert: ' ' trimBoth = ''.
	self assert: '	' trimBoth = ''.
	self assert: 'foo' trimBoth = 'foo'.
	self assert: ' foo ' trimBoth = 'foo'.
	self assert: '  foo  ' trimBoth = 'foo'.
	self assert: ((String with: Character cr), '   foo  ') trimBoth = 'foo'.
	
	self assert: ('aabbaa' trimBoth: [ :each | each = $a ]) = 'bb'.
	self assert: ('bbaabb' trimBoth: [ :each | each = $a ]) = 'bbaabb'