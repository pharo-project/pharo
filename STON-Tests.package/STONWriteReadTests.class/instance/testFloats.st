tests
testFloats
	| floats serialization materialization |
	floats := STON listClass withAll: ((-10 to: 10) collect: [ :each | each * Float pi ]).
	serialization := self serialize: floats.
	materialization := self materialize: serialization.
	self assert: floats size = materialization size.
	1 to: floats size do: [:index | | float |
		float := floats at: index.
		"Use #closeTo: instead of #= to increase portability"
		self assert: (float closeTo: (materialization at: index)) ]