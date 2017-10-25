tests
testAddNil
	| ws |
	ws := WeakSet new.
	ws add: nil.
	self assert: (ws includes: nil)