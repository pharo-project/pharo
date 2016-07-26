tests - indexof
testFindFirstInString
	
	"These tests are more specific than thsoe in testIndexOf."
	
	"more boundary tests"
	self assert: (self findFirstInString: '' fromString: '') = 0.
	self assert: (self findFirstInString: 'x' fromString: '') = 0.
	self assert: (self findFirstInString: '' fromString: 'x') = 0.
	
	self assert: (self findFirstInString: 'x' fromString: 'x') = 1. "<- FIXED"
	
	self assert: (self findFirstInString: 'hello' fromString: 'hello') = 1.
	self assert: (self findFirstInString: 'hello' fromString: 'ello') = 2.
	self assert: (self findFirstInString: 'hello' fromString: 'llo') = 3.
	self assert: (self findFirstInString: 'hello' fromString: 'o') = 5. "<- FIXED"
	self assert: (self findFirstInString: 'hello' fromString: 'x') = 0.
