tests - iterating
testRejectEmpty

	| res |
	res := self empty reject: [:each | each odd].
	self assert: res size = self empty size
	