tests
testUndefinedLastUnary

	| function |
	function := #y ascending undefinedLast.
	self assert: function isUndefinedLast.
	self deny: function isUndefinedFirst.
	self assert: (function value: 0 @ 2 value: 1 @ nil).
	self assert: (function value: 1 @ 2 value: 1 @ 2).
	self deny: (function value: 2 @ 2 value: 1 @ 1).
	function := #y descending undefinedLast.
	self assert: function isUndefinedLast.
	self deny: function isUndefinedFirst.
	self assert: (function value: 1 @ 2 value: 2 @ nil).
	self deny: (function value: 1 @ 2 value: 1 @ 3).
	self deny: (function value: 2 @ 2 value: 1 @ 3)