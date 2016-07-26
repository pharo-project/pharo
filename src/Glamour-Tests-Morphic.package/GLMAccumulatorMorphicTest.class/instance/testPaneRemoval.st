tests
testPaneRemoval
	| browser |
	browser := GLMAccumulator new.
	browser show: [ :a | a list display: [ :x | $a to: x ] ].
	window := browser openOn: $d.
	self assert: browser activeEntity equals: $d.
	browser entity: $c.
	self assert: browser activeEntity equals: $c.
	self tabGroup removePageIndex: 1.
	self assert: self tabGroup tabSelectorMorph tabs size equals: 1.
	self assert: browser activeEntity equals: $c.
	self tabGroup removePageIndex: 1.
	self assert: self tabGroup tabSelectorMorph tabs isEmpty.
	self assert: browser activeEntity isNil.
	browser entity: $c.
	self assert: self tabGroup tabSelectorMorph tabs size equals: 1