tests
testListInside
	| browser firstTreeMorph |
	browser := GLMAccumulator new.
	browser show: [ :a | a list display: [ :x | $a to: x ] ].
	window := browser openOn: $d.
	firstTreeMorph := self find: MorphTreeMorph  in: (self tabGroup pageAt: 1).
	self assert: ((firstTreeMorph roots collect: #complexContents) collect: #item) equals: ($a to: $d) asArray