tests - converting
testTrimRight
	self assert: '' trimRight = ''.
	self assert: ' ' trimRight = ''.
	self assert: '	' trimRight = ''.
	self assert: 'foo' trimRight = 'foo'.
	self assert: ' foo ' trimRight = ' foo'.
	self assert: '  foo  ' trimRight = '  foo'.
	
	self assert: ('aabbaa' trimRight: [ :each | each = $a ]) = 'aabb'.
	self assert: ('bbaabb' trimRight: [ :each | each = $a ]) = 'bbaabb'