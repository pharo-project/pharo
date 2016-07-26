tests
testCachedReturn

	| x |
	self assert: (x := self cachedReturnOfValue) == 6