tests
testPaneCreation
	| browser |
	browser := GLMAccumulator new.
	browser show: [ :a | a list display: [ :x | $a to: x ] ].
	window := browser openOn: $d.
	self assert: self tabGroup pages size equals: 1.
	browser entity: $c.
	self assert: self tabGroup pages size equals: 2