tests
testQuantifierSimple
	"Test quantifier expressions that can be expressed with + or *"
	self assert: ('a'  matchesRegex: 'a{1}').
	self deny:   ('aa' matchesRegex: 'a{1}').
	
	self assert: ('a'  matchesRegex: 'a{1,1}').
	self deny:   ('aa' matchesRegex: 'a{1,1}').
	
	self assert: ('ab'   matchesRegex: '(ab){1,}').
	self assert: ('abab' matchesRegex: '(ab){1,}').
	self deny:   (''     matchesRegex: '(ab){1,}').
	
	self assert: ('ab'   matchesRegex: '(ab){,1}').
	self assert: (''     matchesRegex: '(ab){,1}').
	self deny:   ('abab' matchesRegex: '(ab){,1}').