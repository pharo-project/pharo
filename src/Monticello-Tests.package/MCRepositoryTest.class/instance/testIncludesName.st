tests
testIncludesName
	self deny: (repository includesVersionNamed: 'MonticelloTest-xxx.1-rev1').
	self saveSnapshot1.
	self assert: (repository includesVersionNamed: 'MonticelloTest-xxx.1-rev1').
	self deny: (repository includesVersionNamed: 'MonticelloTest-xxx.1-rev2').
	self saveSnapshot2.
	self assert:  (repository includesVersionNamed: 'MonticelloTest-xxx.1-rev2').