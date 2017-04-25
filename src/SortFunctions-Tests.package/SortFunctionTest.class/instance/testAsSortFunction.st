tests
testAsSortFunction

	| function |
	function := #name ascending.
	self assert: function equals: function asSortFunction.
	function := #name asSortFunction.
	self assert: function equals: function asSortFunction.
	function := [ :each | each name ] ascending.
	self assert: function equals: function asSortFunction.
	function := [ :x :y | x threeWayCompareTo: y ] asSortFunction.
	self assert: function equals: function asSortFunction.