tests
testTwoArgBlock

	| function block |
	block :=
			[:a :b |
			a odd = b odd ifTrue: [0] ifFalse: [a odd ifTrue: [-1] ifFalse: [1]]].
	function := block ascending.
	self assert: function isAscending.
	self deny: function isDescending.
	self assert: (function value: 2 value: 4).
	self assert: (function value: 1 value: 3).
	self assert: (function value: 1 value: 4).
	self deny: (function value: -2 value: 1).
	function := block descending.
	self assert: function isDescending.
	self deny: function isAscending.
	self assert: (function value: 2 value: 4).
	self assert: (function value: 1 value: 3).
	self deny: (function value: 1 value: 4).
	self assert: (function value: -2 value: 1)