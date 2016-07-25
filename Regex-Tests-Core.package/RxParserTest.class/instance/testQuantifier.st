tests
testQuantifier
	self deny:   (''     matchesRegex: 'a{2}').
	self deny:   ('a'    matchesRegex: 'a{2}').
	self assert: ('aa'   matchesRegex: 'a{2}').
	self deny:   ('aaa'  matchesRegex: 'a{2}').
	
	self deny:   (''     matchesRegex: 'a{2,3}').
	self deny:   ('a'    matchesRegex: 'a{2,3}').
	self assert: ('aa'   matchesRegex: 'a{2,3}').
	self assert: ('aaa'  matchesRegex: 'a{2,3}').
	self deny:   ('aaaa' matchesRegex: 'a{2,3}').
	
	self deny:   (''     matchesRegex: 'a{2,}').
	self deny:   ('a'    matchesRegex: 'a{2,}').
	self assert: ('aa'   matchesRegex: 'a{2,}').
	self assert: ('aaa'  matchesRegex: 'a{2,}').
	self assert: ('aaaa' matchesRegex: 'a{2,}').
	
	self assert: (''     matchesRegex: 'a{,3}').
	self assert: ('a'    matchesRegex: 'a{,3}').
	self assert: ('aa'   matchesRegex: 'a{,3}').
	self assert: ('aaa'  matchesRegex: 'a{,3}').
	self deny:   ('aaaa' matchesRegex: 'a{,3}').