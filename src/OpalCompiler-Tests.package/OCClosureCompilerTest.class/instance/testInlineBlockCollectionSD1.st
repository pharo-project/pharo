tests
testInlineBlockCollectionSD1
	| a1 b1 a2 b2 |
	b1 := OrderedCollection new.
	1 to: 3 do:
		[:i |
		a1 := i.
		b1 add: [a1]].
	b1 := b1 asArray collect: [:b | b value].
	b2 := OrderedCollection new.
	1 to: 3 do:
		[:i |
		a2 := i.
		b2 add: [a2]] yourself. "defeat optimization"
	b2 := b2 asArray collect: [:b | b value].
	self assert: b1 = b2