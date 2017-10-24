sunit original tests
testRemove
	| e |
	e := self element.
	full remove: e.
	self assert: (full includes: $w).
	self deny: (full includes: e)