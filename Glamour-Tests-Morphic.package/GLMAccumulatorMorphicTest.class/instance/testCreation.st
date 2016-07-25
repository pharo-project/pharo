tests
testCreation
	| browser |
	browser := GLMAccumulator new.
	browser show: [ :a | a list display: [ :x | $a to: x ] ].
	window := browser openOn: $d.
	self find: LazyTabGroupMorph in: window