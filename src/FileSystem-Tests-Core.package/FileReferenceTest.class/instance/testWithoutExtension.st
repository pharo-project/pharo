tests
testWithoutExtension
	| ref newRef |
	ref := filesystem * 'plonk' / 'griffle.txt'.
	newRef := ref withoutExtension.
	self assert: newRef parent equals: ref parent.
	self assert: newRef basename equals: 'griffle'