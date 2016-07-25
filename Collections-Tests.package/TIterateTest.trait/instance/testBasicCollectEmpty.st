tests - iterating
testBasicCollectEmpty

	| res |
	res := self empty collect: [:each | each class].
	self assert: res isEmpty
	