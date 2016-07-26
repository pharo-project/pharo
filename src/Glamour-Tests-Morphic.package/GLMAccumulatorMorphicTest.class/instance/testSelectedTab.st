tests
testSelectedTab
	| browser |
	browser := GLMAccumulator new.
	browser
		show: [ :a | 
			a title: [ :x | x asString ].
			a list display: [ :x | $a to: x ] ].
	window := browser openOn: $d.
	self assert: self tabGroup selectedPageIndex equals: 1.
	browser entity: $c.
	self assert: self tabGroup selectedPageIndex equals: 2.
	browser entity: $d.
	self assert: self tabGroup selectedPageIndex equals: 1