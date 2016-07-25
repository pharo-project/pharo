tests
testEnabled
	self assert: buttonModel enabled.
	window := buttonModel openWithSpec.
	self assert: self morph enabled.
	buttonModel enabled: false.
	self deny: self morph enabled