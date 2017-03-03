tests
testUndefinedFirstUnary

	| function |
	function := #y ascending undefinedFirst.
	self assert: function isUndefinedFirst.
	self deny: function isUndefinedLast.
	self assert: (function value: 0 @ nil value: 1 @ 1).
	self assert: (function value: 1 @ 2 value: 1 @ 2).
	self deny: (function value: 2 @ 2 value: 1 @ 1).
	function := #y descending undefinedFirst.
	self assert: function isUndefinedFirst.
	self deny: function isUndefinedLast.
	self assert: (function value: 1 @ nil value: 2 @ 1).
	self deny: (function value: 1 @ 2 value: 1 @ 3).
	self deny: (function value: 2 @ 2 value: 1 @ 3)