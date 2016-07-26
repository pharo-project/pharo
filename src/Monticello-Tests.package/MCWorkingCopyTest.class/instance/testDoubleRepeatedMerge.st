tests
testDoubleRepeatedMerge
	| base motherA1 motherA2 motherB1 motherB2 inst |
	base := self snapshot.
	self change: #a toReturn: 'a1'.
	motherA1 := self snapshot.
	self change: #c toReturn: 'c1'.
	motherA2 := self snapshot.
	self load: base.
	self change: #b toReturn: 'b1'.
	motherB1 := self snapshot.
	self change: #d toReturn: 'd1'.
	motherB2 := self snapshot.
	self load: base.
	self merge: motherA1.
	self merge: motherB1.
	self change: #a toReturn: 'a2'.
	self change: #b toReturn: 'b2'.
	self snapshot.
	self merge: motherA2.
	self merge: motherB2.
	inst := self mockInstanceA.
	self assert: inst a = 'a2'.
	self assert: inst b = 'b2'.
	self assert: inst c = 'c1'.
	self assert: inst d = 'd1'