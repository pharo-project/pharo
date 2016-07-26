tests
testActiveEntity
	| browser |
	browser := GLMAccumulator new.
	browser show: [ :a | a list display: [ :x | $a to: x ] ].
	browser entity: $d.
	browser entity: $e.
	window := browser openOn: $d.
	browser entity: $c.
	self tabGroup selectedPageIndex: 1.
	self tabGroup update: #selectedIndex.
	self assert: browser activeEntity equals: $d