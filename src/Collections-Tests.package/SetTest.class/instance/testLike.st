tests - some
testLike
	| e |
	e := self element.
	self assert: (full like: e) equals: e.
	self assert: ((full like: elementNotIn) isNil)