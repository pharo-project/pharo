tests
testReduceRightSpecial
	self assert: ((1 to: 100) reduceRight: [ :a :b | a - b ]) = -50.
	self assert: ('abc' reduceRight: [ :a :b | Array with: a with: b ]) = #($a ($b $c))