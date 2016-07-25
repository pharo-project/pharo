tests
testOnlyOneMorphPerRowInList
	| browser listMorph nodeMorphs|
	"self debug: #testOnlyOneMorphPerRowInList"
	browser := GLMTabulator new.
	browser
		column: #one.
	(browser transmit)
		to: #one;
		andShow: [ :a | a list display: [ :x | x ] ].
	window := browser openOn: (1 to: 100).
	World doOneCycle.
	listMorph := self find: MorphTreeMorph in: window.
	nodeMorphs := (self find: MorphTreeTransformMorph in: listMorph) submorphs.
	self assert: nodeMorphs size equals: 100.
	nodeMorphs do: [:node | "Always fails on node 43" self assert: node submorphs size equals: 1].