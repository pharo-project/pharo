testing
testConflictMethodCreation
	"Generate conflicting methods between t1 and t2
	and check the resulting method in Trait t5 (or c2).
	Also test selectors like foo:x (without space) or selectors with CRs."

	"unary"

	self t2 compile: 'm12 ^false'.
	self assert: ((self t5 sourceCodeAt: #m12) asString beginsWith: 'm12').
	self should: [self c2 new m12] raise: Error.

	"binary"
	self t1 compile: '@ myArgument ^true'.
	self t2 compile: '@myArgument ^false'.
	self 
		assert: ((self t5 sourceCodeAt: #@) asString beginsWith: '@ anObject').
	self should: [self c2 new @ 17] raise: Error.

	"keyword"
	self t1 compile: 'zork: myArgument
		^true'.
	self t2 compile: 'zork: myArgument ^false'.
	self assert: ((self t5 sourceCodeAt: #zork:) asString 
				beginsWith: 'zork: arg1').
	self should: [self c2 new zork: 17] raise: Error.
	self t1 compile: 'zork:myArgument ^true'.
	self t2 compile: 'zork:myArgument ^false'.
	self assert: ((self t5 sourceCodeAt: #zork:) asString 
				beginsWith: 'zork: arg1').
	self should: [self c2 new zork: 17] raise: Error.
	self t1 compile: 'zork1: t1 zork2: t2 ^true'.
	self t2 compile: 'zork1: anObject zork2: anotherObject ^false'.
	self assert: ((self t5 sourceCodeAt: #zork1:zork2:) asString 
				beginsWith: 'zork1: arg1 zork2: arg2').
	self should: [self c2 new zork1: 1 zork2: 2] raise: Error