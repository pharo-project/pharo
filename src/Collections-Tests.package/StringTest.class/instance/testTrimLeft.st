tests - converting
testTrimLeft
	self assert: '' trimLeft = ''.
	self assert: ' ' trimLeft = ''.
	self assert: '	' trimLeft = ''.
	self assert: 'foo' trimLeft = 'foo'.
	self assert: ' foo ' trimLeft = 'foo '.
	self assert: '  foo  ' trimLeft = 'foo  '.
	
	self assert: ('aabbaa' trimLeft: [ :each | each = $a ]) = 'bbaa'.
	self assert: ('bbaabb' trimLeft: [ :each | each = $a ]) = 'bbaabb'