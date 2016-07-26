tests
testStoreOn
	| assoc assoc2 |
	assoc := (0 @ 0 extent: 1 @ 1) -> 0.
	assoc2 := self class compiler evaluate: assoc storeString.
	self assert: assoc key = assoc2 key.
	self assert: assoc value = assoc2 value