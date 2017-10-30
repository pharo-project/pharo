tests
testInversion

	| function |
	function := #x ascending.
	self assert: function isAscending.
	self deny: function isDescending.
	self assert: (function value: 0 @ 2 value: 1 @ 1).
	self assert: (function value: 1 @ 2 value: 1 @ 1).
	self deny: (function value: 2 @ 2 value: 1 @ 1).
	function := function inverted.
	self assert: function isDescending.
	self deny: function isAscending.
	self deny: (function value: 1 @ 2 value: 2 @ 1).
	self assert: (function value: 1 @ 2 value: 1 @ 1).
	self assert: (function value: 2 @ 2 value: 1 @ 1).
	function := function inverted.
	self assert: function isAscending.
	self deny: function isDescending.
	self assert: (function value: 0 @ 2 value: 1 @ 1).
	self assert: (function value: 1 @ 2 value: 1 @ 1).
	self deny: (function value: 2 @ 2 value: 1 @ 1)