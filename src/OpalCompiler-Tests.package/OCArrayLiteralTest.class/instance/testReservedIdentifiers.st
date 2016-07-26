tests
testReservedIdentifiers
	self class compile: 'array ^ #(nil true false)'.
	self assert: self array = {nil. true. false}.