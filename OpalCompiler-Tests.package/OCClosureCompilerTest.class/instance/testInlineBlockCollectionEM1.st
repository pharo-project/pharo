tests
testInlineBlockCollectionEM1
	| a1 b1 i1 a2 b2 i2 we wb |
	b1 := OrderedCollection new.
	i1 := 1.
	[a1 := i1.
	 i1 <= 3] whileTrue:
		[b1 add: [a1].
		i1 := i1 + 1].
	b1 := b1 asArray collect: [:b | b value].
	b2 := OrderedCollection new.
	i2 := 1.
	we := [a2 := i2. i2 <= 3].
	wb := [b2 add: [a2]. i2 := i2 + 1].
	we whileTrue: wb. "defeat optimization"
	b2 := b2 asArray collect: [:b | b value].
	self assert: b1 = b2