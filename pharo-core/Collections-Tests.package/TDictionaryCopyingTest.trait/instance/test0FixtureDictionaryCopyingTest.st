tests - fixture
test0FixtureDictionaryCopyingTest
	| duplicateKey |
	self nonEmptyDict.
	self deny: self nonEmptyDict isEmpty.
	self nonEmptyDifferentFromNonEmptyDict.
	self deny: self nonEmptyDifferentFromNonEmptyDict isEmpty.
	duplicateKey := true.
	self nonEmptyDict keys
		detect: [ :key | self nonEmptyDifferentFromNonEmptyDict includes: key ]
		ifNone: [ duplicateKey := false ].
	self assert: duplicateKey = false