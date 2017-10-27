tests
testReduceLeftSpecial
	self assert: ((1 to: 100) reduceLeft: [ :a :b | a - b ]) = -5048.
	self assert: ('abc' reduceLeft: [ :a :b | Array with: a with: b ]) = #(($a $b) $c)