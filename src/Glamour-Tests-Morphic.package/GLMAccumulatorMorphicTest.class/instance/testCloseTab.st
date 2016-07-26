tests
testCloseTab
	| browser |
	browser := GLMAccumulator new.
	browser
		show: [ :a | 
			a title: [ :x | x asString ].
			a list display: [ :x | $a to: x ] ].
	window := browser openOn: $d.
	browser entity: $c.
	self assert: browser panes size equals: 2.
	self assert: self tabGroup pageMorphs size equals: 2.
	self tabGroup removePageIndex: 1.
	self assert: self tabGroup pageMorphs size equals: 1.
	self assert: browser panes size equals: 1